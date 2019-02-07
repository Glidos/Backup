module Util
( epochTimeToUTCTime
, dropFromEnd
, takeFromEnd
, partialM
, returnFromJust
, fromSingleton
, setMinus
) where

import Data.Bool          (bool)
import Data.Convertible   (convert)
import Data.Time.Clock    (UTCTime)
import Data.List          (sort)
import System.Posix.Types (EpochTime)


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

-- Strip Maybe values, handing Nothing as failure
returnFromJust :: Monad m => String -> Maybe a -> m a
returnFromJust msg = maybe (fail msg) return

-- Return the contents of a singleton
fromSingleton :: [a] -> Maybe a
fromSingleton [v] = Just v
fromSingleton _   = Nothing

-- Order n Log n set minus
setMinus :: Ord a => [a] -> [a] -> [a]
setMinus xs ys = minus (sort xs) (sort ys) where minus [] _ = []
                                                 minus xs [] = xs
                                                 minus (x:xs) (y:ys) = case compare x y of LT -> x:minus xs (y:ys)
                                                                                           EQ -> minus xs (y:ys)
                                                                                           GT -> minus (x:xs) ys

