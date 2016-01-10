{-# Language MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, OverloadedLists #-}
module Rosalyn.ListUtils where

import qualified GHC.Exts

import Prelude hiding (length, head, last, null, tail, map, filter, concat, any, lookup, init, all, foldl, foldr, foldl1, foldr1, maximum, minimum, iterate, span, break, takeWhile, dropWhile, reverse, zip, zipWith, sequence, sequence_, mapM, mapM_, concatMap, and, or, sum, product, repeat, replicate, cycle, take, drop, splitAt, elem, notElem, unzip, lines, words, unlines, unwords)

import qualified Prelude as P

import Data.List (intercalate)

import Data.ListLike

--This module contains utility functions for lists, tuples, and other common constructs.  List functions are defined to work on generic ListLike instances.
--Ideally, all of these can be replaced with existing libraries.

--------
--Tuples

--Map over both elements of a 2-tuple.
--TODO use Template Haskell for this and generalizations.
mapT2 :: (a -> b) -> (a, a) -> (b, b)
mapT2 f (x, y) = (f x, f y)

-------
--Lists

--TODO intercalate should really be part of the ListLike package.  Try to add it there.
--intercalate :: (ListLike l a, ListLike ll l, Monoid l) => l -> ll -> l
--intercalate = concat . intersperse

--bookendIntercalate :: (ListLike l a, ListLike ll l) => l -> l -> l -> ll -> l
bookendIntercalate :: [l] -> [l] -> [l] -> [[l]] -> [l]
bookendIntercalate s e i v = s ++ (intercalate i v) ++ e

--Calculate the cartesian products of (multi)sets a and b.
--Result is lexicographically sorted with respect to a then b.
cartesianProductList :: [a] -> [b] -> [(a, b)]
cartesianProductList [] b = []
cartesianProductList (a:al) b = (map ((,) a) b) ++ (cartesianProduct al b)

cartesianProduct :: (ListLike la a, ListLike lb b, ListLike lab (a, b)) => la -> lb -> lab
cartesianProduct a b = fromList (cartesianProductList (toList a) (toList b))

--Produce a list containing each pair of adjacent items in the input list.  Result is empty if list size <= 1, and of size n - 2 for all other lists.
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs [a] = []
adjacentPairs (a:b:l) = (a, b):(adjacentPairs (b:l))

--Given integers x, y, and a list, take groups of x adjacent values, separated by y dropped values.
alternateTakeDrop :: Int -> Int -> [a] -> [a]
alternateTakeDrop t d [] = []
alternateTakeDrop t d l = (take t l) ++ (alternateTakeDrop t d (drop ((+) t d) l))

disjointAdjacentPairs :: [a] -> [(a, a)]
disjointAdjacentPairs l = zip (alternateTakeDrop 1 1 l) (alternateTakeDrop 1 1 (drop 1 l))


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
      inter = P.concat (P.map (cartesianProduct a) l)
      (intra', inter') = intraInterUnorderedPairs l
   in (intra ++ intra', inter ++ inter')

--TODO name is inconsistent.
--TODO breaks on strict list datatypes.
--indexList :: forall l a li lz . (ListLike l a, ListLike li Int, ListLike lz (Int, a)) => l -> lz
--indexList = zip [0..] --NOTE: This uses OverloadedLists, using the IsList function provided by the ListLike instance (as OverloadedLists uses whatever fromList is in scope), rather than the IsList function provided by the IsList instance. 
--There's some weirdness in here due to the genericity of each list type.

indexList :: forall l a lz . (ListLike l a, ListLike lz (Int, a)) => l -> lz
indexList l =
  let il :: [Int]
      il = [0..]
   in zip il l

countAdjList :: (Eq a) => [a] -> [(a, Int)]
countAdjList [] = []
countAdjList a =
  let countAdj' [] i v = [(v, i)]
      countAdj' (x:l) i v
       | x == v = countAdj' l (succ i) v
       | otherwise = (v, i) : (countAdj' l 1 x)
   in countAdj' (tail a) 1 (head a)

--countAdj :: (Eq a) => [a] -> [(a, Int)]
--countAdj = countAdjList

--TODO is a more efficient generic version possible?
countAdj :: forall l a li . (Eq a, ListLike l a, ListLike li (a, Int)) => l -> li --Requires FlexibleContexts.
countAdj = (fromList . countAdjList . toList)

count :: forall l a li . (Eq a, Ord a, ListLike l a, ListLike li (a, Int)) => l -> li
count = countAdj . sort

--TODO implement lexicographic sort.  Can be more efficient that ordinary sort on lists.
lexicographicSort :: (Ord a) => [a] -> [a]
lexicographicSort = sort

--Merge 2 sorted lists to produce a sorted list consisting of the elements of the concatenation of the original two lists.
mergeList :: (Ord a) => [a] -> [a] -> [a]
mergeList a [] = a
mergeList [] b = b
mergeList a@(ai:al) b@(bi:bl)
 | ai <= bi = ai : (merge al b)
 | otherwise = bi : (merge a bl) 

--TODO can we generalize the concept of converting arguments to lists and converting the result back?  Implementing this for every function would be quite tedious.
--Generic version of merge.
merge :: (Ord a, ListLike l a) => l -> l -> l
merge a b = fromList (merge (toList a) (toList b))

--Very generic version of merge
mergeGeneric :: (Ord a, ListLike l0 a, ListLike l1 a, ListLike l2 a) => l0 -> l1 -> l2
mergeGeneric a b = fromList (merge (toList a) (toList b))

--Returns a list of all lists of the given length consisting of the given elements.
--This function may be thought of as a Cartesian exponent.
--If the input is list is sorted, the output is lexicographically sorted.
--allLists :: (ListLike l a, ListLike ll l) => Int -> l -> ll
--allLists :: (ListLike l a) => Int -> l -> [l]
allLists :: Int -> [a] -> [[a]]
allLists 0 _ = empty
allLists l v = v >>= (\x -> map (cons x) (allLists (pred l) v))

allListsSorted :: (Ord a) => Int -> [a] -> [[a]]
allListsSorted i vals = allLists i (sort vals)

--TODO use TemplateHaskell for these.
allTriples :: [a] -> [(a, a, a)]
allTriples a =
  let --lists :: [[a]]
      lists = allLists 3 a
      listToTriple (a:b:c:[]) = (a, b, c)
   in map listToTriple lists

allQuadrouples :: [a] -> [(a, a, a, a)]
allQuadrouples a =
  let lists = allLists 4 a
      listToQuad (a:b:c:d:[]) = (a, b, c, d)
   in map listToQuad lists

----------
--Sublists

--Trisect a list of length l into ([0, p), [p, p + c), [p + c, l)) 
trisectList :: (ListLike l a) => Int -> Int -> l -> (l, l, l) 
trisectList p c l0 =
  let (s0, l1) = splitAt p l0
      (s1, s2) = splitAt c l1
   in (s0, s1, s2)

--Kmerization
--kmerizeSequence :: Int -> [a] -> [[a]]
kmerizeSequence :: forall ll l a . (ListLike l a, ListLike ll l) => Int -> l -> ll
kmerizeSequence k seq =
  --TODO efficient version of this function using the ListLike interface.
  let t :: ll
      t = tails seq
      partials :: ll
      partials = map (take k) t
   in take ((length t) - k + 1) partials

kmerizeSequenceSet :: forall ll l a . (ListLike l a, ListLike ll l) => Int -> ll -> ll
kmerizeSequenceSet k seqs =
  let kmerLists :: [ll]
      kmerLists = map (kmerizeSequence k) seqs
   in concat kmerLists
      

--Subkmerization (subkmers of length k are kmers of length <= k).
subkmerizeSequence :: forall ll l a. (ListLike l a, ListLike ll l) => Int -> l -> ll
subkmerizeSequence k seq =
  let t :: ll
      t = tails seq
      partials :: [ll]
      partials = map ((take k) . inits) t
   in concat partials

subkmerizeSequenceSet :: forall ll l a . (ListLike l a, ListLike ll l) => Int -> ll -> ll
subkmerizeSequenceSet k seqs =
  let subkmerLists :: [ll]
      subkmerLists = map (subkmerizeSequence k) seqs
   in concat subkmerLists

