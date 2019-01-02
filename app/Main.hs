module Main where

import Backup
import BackupDir
import Data.Tuple.Extra
import Data.Convertible
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Foldable
import Data.Maybe
import Control.Monad
import Control.Monad.Extra
import System.Posix.Types
import System.Posix.Files
import System.Directory.Tree

main :: IO ()
main = do
    todaysBackup <- createTodaysBackup
    ensurePeriodicCopiesOf todaysBackup
    removeOldCopies
    level <- levelToUpdate $ day todaysBackup
    putStrLn $ "Level " ++ show level ++ " missing, not constructable or out of date"
    traverse_ (traverse remove <=< backupForLevel) [level..maxLevelStored]
    unless (level > maxLevelStored) $ void $ createIncrementalCopy level todaysBackup
    previous <- maybe (fail "previous level mysteriously disappeared") return =<< backupForLevel (level - 1)
    diff <- diffBetween previous todaysBackup
    report $ path diff
    compressAndUploadDiff diff
    remove diff
    putStrLn . (++) ("Level " ++ show level ++ " backup complete ") . formatTime defaultTimeLocale "%H:%M:%S" =<< getCurrentTime


createTodaysBackup :: IO Periodic
createTodaysBackup = do
    previous <- backupsForPeriod Daily
    when (null previous) $ fail "no previous backup"
    let mostRecent = maximum previous
    today <- utctDay <$> getCurrentTime
    when (day mostRecent == today) $ fail "backup for today already created"
    createTodayBasedOn mostRecent


ensurePeriodicCopiesOf :: Backup b => b -> IO ()
ensurePeriodicCopiesOf backup =
    let ensureCopyFor freq = unlessM (periodIsRepresented freq backup) $ void $ createPeriodicCopy freq backup
    in traverse_ ensureCopyFor [Weekly, Monthly, Yearly]


numCopies Daily = 7
numCopies Weekly = 4
numCopies Monthly = 12

dropFromEnd :: Int -> [a] -> [a]
dropFromEnd n = take =<< subtract n . length

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
    level1 <- backupForLevel 1 >>= maybe (fail "Level 1 backup missing") return
    days <- daysConstructableFrom (day level1) <$> (filterM remoteOkay =<< remoteDiffs)
    let dayOkayForLevel level d = elem d days && addLifetimeForLevel level d > today
    let levelOkay level = maybe False (dayOkayForLevel level . day) <$> backupForLevel level
    fromJust <$> findM (fmap not . levelOkay) [2..]

showFileShort :: (Integral i, Show i) => String -> i -> String
showFileShort path size = show (quot size (1024 * 1024)) ++ " " ++ path

showFileLong :: String -> FileStatus -> String
showFileLong path fstat = show (fileSize fstat) ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (epochTimeToUTCTime $ modificationTime fstat) ++ path

epochTimeToUTCTime :: EpochTime -> UTCTime
epochTimeToUTCTime = convert

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


-- Given a seed day and a sequence of diffs, return the day of the backup that can be generated.
-- There is no need to check that each diff follows from the preceding one and the first from the
-- seed because this function is used only in cases where that is the case
inferredDay :: Day -> [Diff] -> Day
inferredDay seed [] = seed
inferredDay _ (Diff _ to : _) = to

-- Given a seed day and a set of diffs, return the set of possible inference sequences
-- This allows us to work out what backups can be created from a set of diffs and for
-- each such backup, which of those diffs are needed.
inferences :: Day -> [Diff] -> [[Diff]]
inferences seed diffs = let from (Diff f _) = f
                            extend infs = [d:ds | ds <- infs, d <- diffs, inferredDay seed ds == from d]
                        in concat $ takeWhile (not . null) $ iterate extend [[]]

-- For when we just need to know which are constructable but not how
daysConstructableFrom :: Day -> [Diff] -> [Day]
daysConstructableFrom seed diffs = inferredDay seed <$> inferences seed diffs