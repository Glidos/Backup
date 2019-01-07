module Main where

import Util
import Backup
import BackupDir
import Data.Tuple.Extra
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Foldable
import Data.Maybe
import Control.Monad
import Control.Monad.Extra
import System.Posix.Files
import System.Directory.Tree
import Safe

main :: IO ()
main = do
    todaysBackup <- createTodaysBackup
    ensurePeriodicCopiesOf todaysBackup
    removeOldCopies
    level <- levelToUpdate $ day todaysBackup
    putStrLn $ "Level " ++ show level ++ " missing, not constructable or out of date"
    traverse_ (traverse remove <=< backupForLevel) [level..maxLevelStored]
    unless (level > maxLevelStored) $ void $ createIncrementalCopy level todaysBackup
    previous <- returnFromJust "previous level mysteriously disappeared" =<< backupForLevel (level - 1)
    diff <- diffBetween previous todaysBackup
    report $ path diff
    compressAndUploadDiff diff
    remove diff
    putStrLn . (++) ("Level " ++ show level ++ " backup complete ") . formatTime defaultTimeLocale "%H:%M:%S" =<< getCurrentTime
    putStrLn ""
    checkAndTidyRemoteDiffs $ day todaysBackup


createTodaysBackup :: IO Periodic
createTodaysBackup = do
    mostRecent <- returnFromJust "no previous backup" . maximumMay =<< backupsForPeriod Daily
    today <- utctDay <$> getCurrentTime
    when (day mostRecent == today) $ fail "backup for today already created"
    createBackupForDayBasedOn today mostRecent


ensurePeriodicCopiesOf :: Backup b => b -> IO ()
ensurePeriodicCopiesOf backup =
    let ensureCopyFor freq = unlessM (periodIsRepresented freq backup) $ void $ createPeriodicCopy freq backup
    in traverse_ ensureCopyFor [Weekly, Monthly, Yearly]


numCopies Daily = 7
numCopies Weekly = 4
numCopies Monthly = 12

removeOldCopies :: IO ()
removeOldCopies =
    let removeOldCopiesFor freq =  traverse_ remove =<< dropFromEnd (numCopies freq) . sort <$> backupsForPeriod freq
    in traverse_ removeOldCopiesFor [Daily, Weekly, Monthly]

-- We use 4 levels of incremental backup. There not being a 5th means we never
-- construct diffs wirh respect to the 4th, so only upto the 3rd needs storing
-- locally
maxLevelStored = 3

addLifetimeForLevel :: Integer -> Day -> Day
addLifetimeForLevel 2 = addGregorianMonthsRollOver 1
addLifetimeForLevel 3 = addDays 7
addLifetimeForLevel 4 = addDays 1

-- Find the first level at which we either have no incremental copy, or the backup for that
-- day isn't constructabke from the Level 1 backup and the remote diffs, or the backup is out
-- of date
levelToUpdate :: Day -> IO Integer
levelToUpdate today = do
    level1 <- returnFromJust "Level 1 backup missing" =<< backupForLevel 1
    days <- daysConstructableFrom (day level1) <$> (filterM remoteOkay =<< remoteDiffs)
    let dayOkayForLevel level d = elem d days && addLifetimeForLevel level d > today
    let levelOkay level = maybe False (dayOkayForLevel level . day) <$> backupForLevel level
    fromJust <$> findM (fmap not . levelOkay) [2..]

showFileShort :: (Integral i, Show i) => String -> i -> String
showFileShort path size = show (quot size (1024 * 1024)) ++ " " ++ path

showFileLong :: String -> FileStatus -> String
showFileLong path fstat = show (fileSize fstat) ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (epochTimeToUTCTime $ modificationTime fstat) ++ path

-- Report the overall space used by a directory and enumerate files over 8MB
report :: String -> IO ()
report dpath = do
    _ :/ tree <- readDirectoryWith return dpath
    putStrLn . showFileShort dpath =<< sum <$> traverse (fmap fileSize . getFileStatus) tree
    let printLargeFileInfo fpath = do fstat <- getFileStatus fpath
                                      when (fileSize fstat > 8 * 1024 * 1024) (putStrLn $ showFileLong fpath fstat)
    traverse_ printLargeFileInfo tree


compressAndUploadDiff :: BackupDir b => b -> IO ()
compressAndUploadDiff diff = do
    archivePath <- compress diff
    putStrLn . showFileShort archivePath =<< fileSize <$> getFileStatus archivePath
    makeHash diff
    putStrLn . ("Starting upload " ++) . formatTime defaultTimeLocale "%H:%M:%S" =<< getCurrentTime
    upload diff
    removeArchive diff


checkAndTidyRemoteDiffs :: Day -> IO ()
checkAndTidyRemoteDiffs today = do
    level1 <- returnFromJust "Level 1 backup missing" =<< backupForLevel 1
    allDiffs <- remoteDiffs
    -- generate the inferences for the 2 most recent inferable backups
    let infs = takeFromEnd 2 $ sortOn (inferredDay $ day level1) $ inferences (day level1) allDiffs
    let days = map (inferredDay $ day level1) infs
    unless (today `elem` days) $ fail "Cannot generate todays backup"
    putStrLn $ unwords $ "Most recent backup days:" : map show days
    putStrLn $ "Level 1: " ++ display level1
    let requiredDiffs = nub $ concat infs
    requiredDiffsOkay <- traverse remoteOkay requiredDiffs
    let reportDiff diff okay = display diff ++ " " ++ if okay then "ok" else "corrupt"
    putStr $ unlines $ "Keeping files:" : zipWith reportDiff requiredDiffs requiredDiffsOkay
    unless (and requiredDiffsOkay) $ fail "A required diff is corrupt"
    let toDelete = allDiffs \\ requiredDiffs
    putStr $ unlines $ "Deleting:" : map display toDelete
    removeRemotes toDelete


-- Given a seed day and a sequence of diffs, return the day of the backup that can be generated.
-- There is no need to check that each diff follows from the preceding one and the first from the
-- seed because this function is used only in cases where that is the case
inferredDay :: Day -> [Diff] -> Day
inferredDay seed [] = seed
inferredDay _ (diff:_) = toDay diff

-- Given a seed day and a set of diffs, return the set of possible inference sequences
-- This allows us to work out what backups can be created from a set of diffs and for
-- each such backup, which of those diffs are needed.
inferences :: Day -> [Diff] -> [[Diff]]
inferences seed diffs = let extend infs = [d:inf | inf <- infs, d <- diffs, inferredDay seed inf == fromDay d]
                        in concat $ takeWhile (not . null) $ iterate extend [[]]

-- For when we just need to know which are constructable but not how
daysConstructableFrom :: Day -> [Diff] -> [Day]
daysConstructableFrom seed diffs = inferredDay seed <$> inferences seed diffs