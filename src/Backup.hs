module Backup
( Frequency(..)
, Periodic(..)
, Incremental(..)
, Diff(..)
, Backup(..)
, backupsForPeriod
, backupForLevel
, createBackupForDayBasedOn
, createPeriodicCopy
, createIncrementalCopy
, periodIsRepresented
, diffBetween
, remoteDiffs
) where

import Data.Time.Calendar    (Day)
import Data.Time.Format      (defaultTimeLocale, formatTime, parseTimeM)
import Data.Maybe            (mapMaybe)
import Data.List             (intercalate)
import BackupDir             (BackupDir, subDir, wrapperDir, baseDir, path, outerPath, remoteItems)
import System.Directory      (doesDirectoryExist, createDirectoryIfMissing, listDirectory)
import System.FilePath.Posix ((</>), takeDirectory, takeFileName)
import System.Process.Typed  (runProcess_, shell)
import Control.Monad         ((<=<))
import Text.Regex            (mkRegex, matchRegex)

import Util                  (returnFromJust, partialM)

data Frequency = Daily | Weekly | Monthly | Yearly deriving (Eq, Ord)

data Periodic = Periodic Frequency Day deriving (Eq, Ord)
data Incremental = Incremental Integer Day
data Diff = Diff {fromDay :: Day, toDay :: Day} deriving Eq

instance Show Periodic where
    show (Periodic _ day) = show day

instance Show Incremental where
    show (Incremental _ day) = show day

instance Show Diff where
    show (Diff from to) = show from ++ "to" ++ show to

-- Daily backups are held as directories with the names matching the precise date
-- Copies held for longer periods are held as directories with names matching
-- the week, month or year and with a subdirectory named to match the precise
-- date.
instance BackupDir Periodic where
    subDir _ = Nothing
    wrapperDir (Periodic freq day) = case freq of Daily -> Nothing
                                                  _     -> Just $ formatDay freq day

instance BackupDir Incremental where
    subDir _ = Nothing
    wrapperDir (Incremental level _) = Just $ formatLevel level

instance BackupDir Diff where
    subDir _ = Just "Diffs"
    wrapperDir _ = Nothing


class BackupDir b => Backup b where
    day :: b -> Day

instance Backup Periodic where
    day (Periodic _ d) = d

instance Backup Incremental where
    day (Incremental _ d) = d


dayFormatString :: Frequency -> String
dayFormatString Daily = "%Y-%m-%d"
dayFormatString Weekly = "%Y-Week%W"
dayFormatString Monthly = "%Y-%m"
dayFormatString Yearly = "%Y"

formatDay :: Frequency -> Day -> String
formatDay freq = formatTime defaultTimeLocale (dayFormatString freq)

parseDay :: Frequency -> String -> Maybe Day
parseDay freq = parseTimeM True defaultTimeLocale (dayFormatString freq)

testParseDay :: Frequency -> String -> Bool
testParseDay freq = (/= Nothing) . parseDay freq


formatLevel :: Integer -> String
formatLevel level = "Level" ++ show level

fromSingleton :: [a] -> Maybe a
fromSingleton [v] = Just v
fromSingleton _   = Nothing

-- Derive the day from a backup path. For backup copies that have an outer wrapper directory,
-- we cannot derive the day from the wrapper directory's name. We have to look within for a
-- single subdirectory named according to the day. (Multiple subdirectories would be a sign
-- of something having gone wrong).
dayForPath :: Bool -> String -> IO Day
dayForPath useSubdir path = returnFromJust ("Corrupt backup: " ++ path) . (parseDay Daily =<<) =<< (if useSubdir then fromSingleton <$> listDirectory path else return $ Just $ takeFileName path)

-- List the current periodic backups for a specific frequency
--
-- Find the directories that match the pattern for the frequency. For daily, derive
-- the day from the directory name, otherwise look inside for a single directory from
-- which to derive the day.
backupsForPeriod :: Frequency -> IO [Periodic]
backupsForPeriod freq = traverse ((Periodic freq <$>) . dayForPath (freq /= Daily) . (baseDir </>)) . filter (testParseDay freq) =<< listDirectory baseDir

-- Return the current Incremental backkup for a level if present
backupForLevel :: Integer -> IO (Maybe Incremental)
backupForLevel level = traverse ((Incremental level <$>) . dayForPath True) =<< partialM doesDirectoryExist (baseDir </> formatLevel level)

backupTarget = "/"

-- Create a backup for today based on an exisiting backup
-- Unchanged files will share disc space with the existing backup
createBackupForDayBasedOn :: Backup b => Day -> b -> IO Periodic
createBackupForDayBasedOn day previous = let backup = Periodic Daily day
                                         in runProcess_ (shell $ intercalate "&&" ["rsync -ra --files-from=rsync-list --exclude-from=backup-exclude --link-dest=" ++ path previous ++ " " ++ backupTarget ++ " " ++ path backup,
                                                                                   "cd " ++ path backup,
                                                                                   "find . > file_list"])
                                            >> return backup

-- Create a copy of a backup, sharing disc space
createCopy :: (Backup b, Backup c) => b -> c -> IO ()
createCopy base backup = createDirectoryIfMissing False (outerPath backup)
                         >> runProcess_ (shell $ "cp -al " ++ path base ++ " " ++ path backup)


-- For a given frequency, create a periodic copy of an existing backup,
-- sharing disc space
createPeriodicCopy :: Backup b => Frequency -> b -> IO Periodic
createPeriodicCopy freq base = createCopy base bk >> return bk where bk = Periodic freq $ day base

-- For a given level, create an incremental copy of an existing backup,
-- sharing disc space
createIncrementalCopy :: Backup b => Integer -> b -> IO Incremental
createIncrementalCopy level base = createCopy base bk >> return bk where bk = Incremental level $ day base

-- For an existing backup and a frequency, test whether there is a corresponding
-- periodic copy. We don't expect a copy for the specific day; we just wish to
-- know if the period is represented, so we check the outer path.
periodIsRepresented :: Backup b => Frequency -> b -> IO Bool
periodIsRepresented freq base = doesDirectoryExist $ outerPath $ Periodic freq $ day base

parseDiff :: String -> Maybe Diff
parseDiff = fmap (\[a,b] -> Diff a b) . traverse (parseDay Daily) <=< matchRegex (mkRegex "^(.*)to(.*)$")

-- List the remotely stored diffs
remoteDiffs :: IO [Diff]
remoteDiffs = mapMaybe parseDiff <$> remoteItems


-- On the basis of two backups, pull out just the files that are in the
-- second but not the first or that have changed
diffBetween ::  (Backup b, Backup c) => b -> c -> IO Diff
diffBetween from to = let diff = Diff (day from) (day to)
                      in createDirectoryIfMissing False (takeDirectory $ path diff)
                         >> runProcess_ (shell $ "rsync -ra --exclude-from=upload-exclude --compare-dest=" ++ path from ++ " " ++ path to ++ " " ++ path diff)
                         >> return diff
