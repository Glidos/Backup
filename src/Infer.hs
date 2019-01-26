module Infer
( inferredDay
, inferences
, daysConstructableFrom
) where

import Data.Time.Calendar (Day)

import Backup             (Diff, toDay, fromDay)

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
