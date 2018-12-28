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
constructable :: Day -> [Diff] -> [Day]
constructable seed diffs = inferredDay seed <$> inferences seed diffs