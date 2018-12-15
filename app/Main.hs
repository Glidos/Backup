module Main where

import Backup

main :: IO ()
main = do
    path <$> today >>= print
    path . copyForPeriod Weekly <$> today >>= print
    path . copyForPeriod Monthly <$> today >>= print
    path . copyForPeriod Yearly <$> today >>= print
    path . copyForLevel 1 <$> today >>= print
    from <- copyForLevel 1 <$> today
    to <- copyForLevel 2 <$> today
    print $ path $ diffFromTo from to
