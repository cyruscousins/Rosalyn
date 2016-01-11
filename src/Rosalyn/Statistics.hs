module Rosalyn.Statistics where

import Rosalyn.Sequence
import Rosalyn.ListUtils

import Data.Ratio
import Data.List
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set

---------------------
--Assembly Statistics

--Length statistics:

--Input: Ratio parameterizing the statistic, estimated genome size, sizes.
--Generalization of the NG50 statistic.
ngxStatistic :: Ratio Int -> Int -> [Int] -> Ratio Int
ngxStatistic x g l = 
  let partials = map (\ x -> (sum x, last x)) ((tail . inits) l) --TODO replace this with a fold or partial collecting fold.
      nx' :: [(Int, Int)] -> Ratio Int
      nx' [(s, v)] = v % 1 --If we get to the last item, it's the N50.
      nx' ((s, v):(s', v'):r)
       | (s % g) == x = (v + v') % 2--If exactly x weight comes before, then we take the average of this value and the next.
       | (s % g) > x = v % 1
       | otherwise = nx' ((s', v'):r)
   in nx' partials

--Generalization of the N50 statistic.
nxStatistic :: Ratio Int -> [Int] -> Ratio Int
nxStatistic x l = ngxStatistic x (sum l) l

--Generalization of the L50 statistic.
--lxStatistic :: Ratio Int -> [Int] -> Ratio Int --TODO

--Generalization of the D50 statistic.
--dxStatistic :: Ratio Int -> [Int] -> Int

n50Statistic :: [Int] -> Ratio Int
n50Statistic = nxStatistic (1 % 2)

nxStatisticLength :: Ratio Int -> [[a]] -> Ratio Int
nxStatisticLength r = (nxStatistic r) . (map length)

n50StatisticLength :: [[a]] -> Ratio Int
n50StatisticLength = n50Statistic . (map length)

--Kmer statistics

jacardKmerAssemblyCoverage :: Int -> Genome -> ReadSet -> Ratio Int
jacardKmerAssemblyCoverage i a b = jacardDistance (Set.fromList $ kmerizeSequence i a) (Set.fromList $ kmerizeSequenceSet i b)

jacardSubkmerAssemblyCoverage :: Int -> Genome -> ReadSet -> Ratio Int
jacardSubkmerAssemblyCoverage i a b = jacardDistance (Set.fromList $ subkmerizeSequence i a) (Set.fromList $ subkmerizeSequenceSet i b)

jacardKmerCoverage :: Int -> Sequence -> Sequence -> Ratio Int
jacardKmerCoverage k a b = jacardKmerAssemblyCoverage k a [b]

jacardSubkmerCoverage :: Int -> Sequence -> Sequence -> Ratio Int
jacardSubkmerCoverage k a b = jacardSubkmerAssemblyCoverage k a [b]
-------------------
--Jacard Statistics

jacardSimilarity :: (Ord a) => Set.Set a -> Set.Set a -> Ratio Int
jacardSimilarity a b = (%) (Set.size (Set.intersection a b)) (Set.size (Set.union a b))

jacardDistance :: (Ord a) => Set.Set a -> Set.Set a -> Ratio Int
jacardDistance a b = 1 - (jacardSimilarity a b)

--Generalized Jacard Similarity.
generalizedJacardSimilarity :: (Ord a) => MultiSet.MultiSet a -> MultiSet.MultiSet a -> Ratio Int
generalizedJacardSimilarity a b =
  let mins  = MultiSet.intersection a b
      maxes = MultiSet.union a b
   in (%) (MultiSet.size mins) (MultiSet.size maxes)

generalizedJacardDistance :: (Ord a) => MultiSet.MultiSet a -> MultiSet.MultiSet a -> Ratio Int
generalizedJacardDistance a b = 1 - (generalizedJacardSimilarity a b)

--------------------
--Kolmogorov-Smirnov

--https://hackage.haskell.org/package/statistics-0.13.2.3/docs/Statistics-Test-KolmogorovSmirnov.html

--TODO coefficient of determination.

-------
--AUROC

--TODO this is horribly inefficient: there is an O(a + b) algorithm, but this is O(ab)
auroc :: (Ord o) => [o] -> [o] -> Ratio Int
auroc a b =
  let pairs = cartesianProduct a b
      indicators :: [Int]
      indicators = map (\ (a, b) -> if a < b then 1 else 0) pairs
   in (fromIntegral $ sum indicators) % (fromIntegral $ length pairs)

