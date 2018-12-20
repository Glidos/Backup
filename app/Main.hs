module Main where

import Backup
import BackupDir
import Data.Tuple.Extra

bothPaths :: BackupDir b => b -> (String, String)
bothPaths = path &&& outerPath

main :: IO ()
main = do
    print . map path =<< backupsForPeriod Daily
    print . map path =<< backupsForPeriod Weekly
    print . map path =<< backupsForPeriod Monthly
    print . map path =<< backupsForPeriod Yearly
    print . map (fmap path) =<< traverse backupForLevel [1..4]