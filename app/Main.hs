module Main where

import Backup
import BackupDir
import Data.Tuple.Extra
import Data.List
import Data.Time.Clock
import Data.Foldable
import Control.Monad
import Control.Monad.Extra

main :: IO ()
main = do
    today <- createTodaysBackup
    ensurePeriodicCopies today
    removeOldCopies


createTodaysBackup :: IO Periodic
createTodaysBackup = do
    previous <- backupsForPeriod Daily
    when (null previous) $ fail "no previous backup"
    let mostRecent = maximum previous
    today <- utctDay <$> getCurrentTime
    when (day mostRecent == today) $ fail "backup for today already created"
    createTodayBasedOn mostRecent


ensurePeriodicCopies :: Backup b => b -> IO ()
ensurePeriodicCopies backup =
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