module Util
( epochTimeToUTCTime
, dropFromEnd
, takeFromEnd
, partialM
) where

import Data.Bool
import Data.Convertible
import Data.Time.Clock
import System.Posix.Types


epochTimeToUTCTime :: EpochTime -> UTCTime
epochTimeToUTCTime = convert

-- Version of drop that works from the end of the list
dropFromEnd :: Int -> [a] -> [a]
dropFromEnd n = take =<< subtract n . length

-- Version of take that works from the end of the list
takeFromEnd :: Int -> [a] -> [a]
takeFromEnd n = drop =<< subtract n . length

-- Version of partial that works in a monadic context
partialM :: Monad m => (a -> m Bool) -> a -> m (Maybe a)
partialM p x = bool Nothing (Just x) <$> p x

