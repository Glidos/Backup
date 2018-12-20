module Backup
( Frequency(..)
, display
, path
, outerPath
, backupsForPeriod
, backupForLevel
) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Maybe
import Data.Bool
import BackupDir
import System.Directory
import System.FilePath.Posix
import Control.Monad

data Frequency = Daily | Weekly | Monthly | Yearly deriving Eq

data Periodic = Periodic Frequency Day
data Incremental = Incremental Integer Day
data Diff = Diff Day Day

-- Daily backups are held as directories with the names matching the precise date
-- Copies held for longer periods are held as directories with names matching
-- the week, month or year and with a subdirectory named to match the precise
-- date.
instance BackupDir Periodic where
    display (Periodic _ day) = show day
    subDir _ = Nothing
    wrapperDir (Periodic freq day) = case freq of Daily -> Nothing
                                                  _     -> Just $ formatDay freq day

instance BackupDir Incremental where
    display (Incremental _ day) = show day
    subDir _ = Nothing
    wrapperDir (Incremental level _) = Just $ formatLevel level

instance BackupDir Diff where
    display (Diff from to) = show from ++ "to" ++ show to
    subDir _ = Just "Diffs"
    wrapperDir _ = Nothing


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

-- Derive the day from a backup path
dayForPath :: Bool -> String -> IO Day
dayForPath useSubdir path =
    do  let errorMessage = "Corrupt backup: " ++ path
        dayString <- if useSubdir then do subdirs <- listDirectory path
                                          case subdirs of [subdir] -> return subdir
                                                          _        -> fail errorMessage
                                  else return $ takeFileName path
        case parseDay Daily dayString of Just day -> return day
                                         Nothing  -> fail errorMessage

-- List the current periodic backups for a specific frequency
--
-- Find the directories that match the pattern for the frequency. For daily, derive
-- the day from the directory name, otherwise look inside for a single directory from
-- which to derive the day.
backupsForPeriod :: Frequency -> IO [Periodic]
backupsForPeriod freq = traverse ((Periodic freq <$>) . dayForPath (freq /= Daily) . (baseDir </>)) . filter (testParseDay freq) =<< listDirectory baseDir

partialM :: Monad m => (a -> m Bool) -> a -> m (Maybe a)
partialM p x = bool Nothing (Just x) <$> p x

-- Return the current Incremental backkup for a level if present
backupForLevel :: Integer -> IO (Maybe Incremental)
backupForLevel level = traverse ((Incremental level <$>) . dayForPath True) =<< partialM doesDirectoryExist (baseDir </> formatLevel level)

-- Create a backup for today based on an exisiting backup
-- Unchanged files will share disc space with the existing backup
-- createTodayBasedOn :: Periodic -> IO Periodic

-- For a given frequency, create a periodic copy of an existing backup,
-- sharing disc space
-- copyForPeriod :: Frequency -> Periodic -> IO Periodic

-- For a given level, create an incremental copy of an existing backup,
-- sharing disc space
-- copyForLevel :: Integer -> Periodic -> IO Incremental

-- On the basis of two backups, pull out just the files that are in the
-- second but not the first or that have changed
-- diffBetween ::  Incremental -> Incremental -> IO Diff