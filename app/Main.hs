module Main where

import Backup
import BackupDir
import Data.Tuple.Extra

bothPaths :: BackupDir b => b -> (String, String)
bothPaths = path &&& outerPath

main :: IO ()
main = do
    bothPaths <$> today >>= print
    bothPaths . copyForPeriod Weekly <$> today >>= print
    bothPaths . copyForPeriod Monthly <$> today >>= print
    bothPaths . copyForPeriod Yearly <$> today >>= print
    bothPaths . copyForLevel 1 <$> today >>= print
    from <- copyForLevel 1 <$> today
    to <- copyForLevel 2 <$> today
    print $ bothPaths $ diffFromTo from to
