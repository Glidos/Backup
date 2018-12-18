module Backup
    (
        Period(..),
        display,
        path,
        today,
        copyForPeriod,
        copyForLevel,
        diffFromTo
    ) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import BackupDir

data Period = Daily | Weekly | Monthly | Yearly

data Periodic = Periodic Period Day
data Incremental = Incremental Integer Day
data Diff = Diff Day Day

instance BackupDir Periodic where
    display (Periodic _ day) = show day
    subDir (Periodic period day) = case period of Daily -> Nothing
                                                  _ -> Just $ format period day

instance BackupDir Incremental where
    display (Incremental _ day) = show day
    subDir (Incremental level _) = Just $ "Level" ++ show level

instance BackupDir Diff where
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