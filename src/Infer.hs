module Infer
( inferences,
  steps
) where


inferences :: Eq a => a -> [(a,a)] -> [[a]]
inferences seed pairs = let extend infs = [to:inf | inf <- infs, (from,to) <- pairs, head inf == from]
                        in concat $ takeWhile (not . null) $ iterate extend [[seed]]

steps :: [a] -> [(a,a)]
steps [_] = []
steps (x1:x2:xs) = (x1,x2):steps(x2:xs)
