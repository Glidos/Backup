module Backup
    (
        Period(..),
        display,
        path,
        outerPath,
        today,
        copyForPeriod,
        copyForLevel,
        diffFromTo
    ) where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format

baseDir = "/home/backup"

data Period = Daily | Weekly | Monthly | Yearly

data Periodic = Periodic Period Day
data Incremental = Incremental Integer Day
data Diff = Diff Day Day

class Backup a where
    display :: a -> String
    subDir :: a -> Maybe String -- Some backup copies are placed in subdirectories
    path :: a -> String -- Where the backup is kept
    outerPath :: a -> String -- What to delete to remove the backup
    outerPath bk = baseDir ++ "/" ++ case subDir bk of Nothing -> display bk
                                                       Just dir -> dir
    path bk = case subDir bk of Nothing -> outerPath bk
                                Just _ -> outerPath bk ++ "/" ++ display bk

instance Backup Periodic where
    display (Periodic _ day) = show day
    subDir (Periodic period day) = case period of Daily -> Nothing
                                                  _ -> Just $ format period day

instance Backup Incremental where
    display (Incremental _ day) = show day
    subDir (Incremental level _) = Just $ "Level" ++ show level

instance Backup Diff where
    display (Diff from to) = show from ++ "to" ++ show to
    subDir _ = Nothing
    outerPath bk = baseDir ++ "/Diffs/" ++ display bk
 
formatString :: Period -> String
formatString Daily = "%Y-%m-%d"
formatString Weekly = "%Y-Week%W"
formatString Monthly = "%Y-%m"
formatString Yearly = "%Y"

format :: Period -> Day -> String
format period = formatTime defaultTimeLocale (formatString period)

today :: IO Periodic
today = Periodic Daily . utctDay <$> getCurrentTime

copyForPeriod :: Period -> Periodic -> Periodic
copyForPeriod period (Periodic _ day) = Periodic period day

copyForLevel :: Integer -> Periodic -> Incremental
copyForLevel level (Periodic _ day) = Incremental level day

diffFromTo :: Incremental -> Incremental -> Diff
diffFromTo (Incremental _ from) (Incremental _ to) = Diff from to