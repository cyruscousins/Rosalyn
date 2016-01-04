module Rosalyn.ListUtils where

import Data.List

--This module contains utility functions for lists, tuples, and other common constructs.
--Ideally, all of these can be replaced with existing libraries.

--------
--Tuples

--Map over both elements of a 2-tuple.
mapT2 :: (a -> b) -> (a, a) -> (b, b)
mapT2 f (x, y) = (f x, f y)

-------
--Lists

--Calculate the cartesian products of (multi)sets a and b.
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct [] b = []
cartesianProduct (a:al) b = (map ((,) a) b) ++ (cartesianProduct al b)

--Produce a list containing each pair of adjacent items in the input list.  Result is empty if list size <= 1, and of size n - 2 for all other lists.
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs [a] = []
adjacentPairs (a:b:l) = (a, b):(adjacentPairs (b:l))

--Produce a list containing all unordered unique pairs.
--Contains a maximal set of (a, b) such that (b, a) is not also included.
allPairsUnordered :: [a] -> [(a, a)]
allPairsUnordered (a:b:l) = (:) (a, b) ((map ((,) a) l) ++ (allPairsUnordered ((:) b l)))
allPairsUnordered _ = []

intPairsUnordered :: Int -> Int -> [(Int, Int)]
intPairsUnordered l h = allPairsUnordered [l..h]

allPairsUnorderedIndexed :: [a] -> [((Int, a), (Int, a))]
allPairsUnorderedIndexed l =
  let indexPairs = intPairsUnordered 0 (pred $ length l)
   in map (\(i, j) -> ((i, (!!) l i), (j, (!!) l j))) indexPairs

intraInterUnorderedPairs :: [[a]] -> ([(a, a)], [(a, a)])
intraInterUnorderedPairs [] = ([], [])
intraInterUnorderedPairs (a:l) =
  let intra = allPairsUnordered a
      inter = concat (map (cartesianProduct a) l)
      (intra', inter') = intraInterUnorderedPairs l
   in (intra ++ intra', inter ++ inter')

indexList :: [a] -> [(Int, a)]
indexList = zip [0..]

countAdj :: (Eq a) => [a] -> [(a, Int)]
countAdj [] = []
countAdj a =
  let countAdj' [] i v = [(v, i)]
      countAdj' (x:l) i v
       | x == v = countAdj' l (succ i) v
       | otherwise = (v, i) : (countAdj' l 1 x)
   in countAdj' (tail a) 1 (head a)

count :: (Eq a, Ord a) => [a] -> [(a, Int)]
count = sort . countAdj

--TODO implement lexicographic sort.  Can be efficient that ordinary sort on lists.
lexicographicSort :: (Ord a) => [a] -> [a]
lexicographicSort = sort

--Merge 2 sorted lists to produce a sorted list consisting of the elemnents of the concatenation of the original two lists.
merge :: (Ord a) => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge a@(ai:al) b@(bi:bl)
 | ai <= bi = ai : (merge al b)
 | otherwise = bi : (merge a bl) 

