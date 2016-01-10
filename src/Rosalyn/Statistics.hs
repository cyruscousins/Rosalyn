module Rosalyn.Statistics where

import Data.Ratio
import Data.List

nxStatistic :: Ratio Int -> [Int] -> Ratio Int
nxStatistic x l =
  let total = sum l
      partials = map (\ x -> (sum x, last x)) ((tail . inits) l) --TODO replace this with a fold or partial collecting fold.
      nx' :: [(Int, Int)] -> Ratio Int
      nx' [(s, v)] = v % 1 --If we get to the last item, it's the N50.
      nx' ((s, v):(s', v'):r)
       | (s % total) == x = (v + v') % 2--If exactly x weight comes before, then we take the average of this value and the next.
       | (s % total) > x = v % 1
       | otherwise = nx' ((s', v'):r)
   in nx' partials
{-
  let counts = countAdj l
      weightedSizes = map ( \ (s, c) -> (s, s * c)) counts
      total = sum (map snd weightedSizes)
      partials = map (\ l -> (sum (map snd l), last (map fst l))) (heads total)
      n50' [(s, c)] = s --Last item is N50 (special case, because it can't possibly be the average of 2).
-}

n50Statistic :: [Int] -> Ratio Int
n50Statistic = nxStatistic (1 % 2)

nxStatisticLength :: Ratio Int -> [[a]] -> Ratio Int
nxStatisticLength r = (nxStatistic r) . (map length)

n50StatisticLength :: [[a]] -> Ratio Int
n50StatisticLength = n50Statistic . (map length)
