{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Rosalyn.Trees where

import GHC.Exts

import Rosalyn.Random
import Rosalyn.Distance
import Rosalyn.ListUtils

import Data.Tuple
import Data.Maybe
import qualified Data.Graph

type List a = [a]

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

type WeightedBinaryTree a = BinaryTree (a, Double)

instance Functor BinaryTree where
  fmap f (Node a c0 c1) = (Node (f a) (fmap f c0) (fmap f c1))
  fmap f Empty = Empty

instance (Show a) => Show (BinaryTree a) where
  show Empty = ""
  --show (Node a c0 c1) = "((\"" ++ (show a) ++ "\") (" ++ (show c0) ++ ") (" ++ (show c1) ++ "))"
  show (Node a c0 c1) = "(\"" ++ (show a) ++ "\"" ++ (show c0) ++ (show c1) ++ ")"

isLeaf :: BinaryTree a -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

--Map over parts of the tree that meet some predicate.
treeMapConditional :: (BinaryTree a -> Bool) -> (a -> a) -> BinaryTree a -> BinaryTree a
treeMapConditional _ _ Empty = Empty
treeMapConditional c f n@(Node v c0 c1) =
  let b = c n
      v' = if b then f v else v
      c0' = treeMapConditional c f c0
      c1' = treeMapConditional c f c1
   in Node v' c0' c1'

--Contract any node with exactly one empty child.
treeContract :: BinaryTree a -> BinaryTree a
treeContract Empty = Empty
treeContract (Node v c0@(Node _ _ _) c1@(Node _ _ _)) = Node v (treeContract c0) (treeContract c1)
treeContract n@(Node _ Empty Empty) = n
treeContract (Node _ Empty a) = treeContract a
treeContract (Node _ a Empty) = treeContract a

--Map all leaves, leaving internal nodes unchanged.
treeMapLeaf :: (a -> a) -> BinaryTree a -> BinaryTree a
treeMapLeaf = treeMapConditional isLeaf

--Map internal nodes, leaving all leaves unchanged.
treeMapInternal :: (a -> a) -> BinaryTree a -> BinaryTree a
treeMapInternal = treeMapConditional (not . isLeaf)

--Drop internal nodes (leaving only leaves).
dropInternal :: BinaryTree a -> BinaryTree (Maybe a)
dropInternal t = treeMapInternal (const Nothing) (fmap Just t)

--Note: these functions are not intended to be balancing!
bstInsert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
bstInsert Empty a = Node a Empty Empty
bstInsert n@(Node a0 lc rc) a1
  | (==) a1 a0 = n
  | (<)  a1 a0 = Node a0 (bstInsert lc a1) rc
  | (>)  a1 a0 = Node a0 lc (bstInsert rc a1)

bstInsertMulti :: (Ord a) => BinaryTree a -> List a -> BinaryTree a
bstInsertMulti n l = foldl (\ t i -> bstInsert t i) n l

bstFromList :: (Ord a) => List a -> BinaryTree a
bstFromList l = bstInsertMulti Empty l

--Produce a random (empty) binary tree structure.
instance Randomizable (BinaryTree ()) where
  sizedRandom size =
    do shuffle <- shuffleList [1..size]
       return (fmap (const ()) (bstFromList shuffle))

--Instance IsList to allow list syntax for binary trees.  Here we assume that elements are ordered, and a binary tree is desired.
instance (Ord a) => IsList (BinaryTree a) where
  type Item (BinaryTree a) = a
  fromList = bstFromList
  toList   = inOrder

--TODO making a BST is somewhat arbitrary.


--TODO can we give an alternative instance of IsList for Tuples and/or lists to allow Lisplike tree definitions?
--TODO can we instance ListLike?

--The zip operation on trees: where tree structure overlaps, combine data.
--TODO is there a typeclass for this?
zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree (Node va lca rca) (Node vb lcb rcb) = Node (va, vb) (zipTree lca lcb) (zipTree rca rcb)

--Zips a tree and a list with in order traversal.
--TODO define better behavior when sizes don't match.
zipTreeList :: BinaryTree a -> [b] -> BinaryTree (a, b)
zipTreeList r vals =
  let ztl' Empty v = (Empty, v)
      ztl' (Node a lc rc) v =
        let (lc', v') = ztl' lc v
            (rc', v'') = ztl' rc (tail v')
            nn = Node (a, head v') lc' rc'
         in (nn, v'')
      (t, _) = ztl' r vals
   in t

--TODO instance traverseable: we absolutely need to sequence and mapM on trees of randoms!

--In order traversal.
inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node v c0 c1) = (inOrder c0) ++ ((:) v (inOrder c1))

--Indices 
indexTree :: BinaryTree a -> [(a, Int)]
indexTree = ((map swap) . (indexList . inOrder))

indexTreeNonempty :: BinaryTree (Maybe a) -> [(Int, a)]
indexTreeNonempty t =
  let ordered = inOrder t
      nonEmpty = map fromJust $ filter isJust ordered
      indexed = indexList nonEmpty
   in indexed

indexTreeInline :: BinaryTree a -> BinaryTree (a, Int)
indexTreeInline r =
  let traverse Empty i = (Empty, i)
      traverse (Node v lc rc) i =
        let (lc', i')  = traverse lc i
            (rc', i'') = traverse rc (succ i')
            in (Node (v, i') lc' rc', i'')
   in fst $ traverse r (0 :: Int)

augmentIndices :: BinaryTree a -> BinaryTree (a, Int)
augmentIndices t =
  let augmentIndices' Empty i = (Empty, i)
      augmentIndices' (Node v lc rc) i =
        let (lc', i')  = augmentIndices' lc i
            (rc', i'') = augmentIndices' rc (succ i')
            n = Node (v, i') lc' rc'
         in (n, i'')
      (augmentedIndices, top) = augmentIndices' t (0 :: Int)
   in augmentedIndices

parentChildPairs :: BinaryTree a -> [(a, a)]
parentChildPairs Empty = []
parentChildPairs (Node r lc rc) =
  let parentChildPairs' p Empty = []
      parentChildPairs' p (Node c lc rc) = (parentChildPairs' c lc) ++ [(p, c)] ++ (parentChildPairs' c rc)
   in (parentChildPairs' r lc) ++ (parentChildPairs' r rc)

--Convert a binary tree to a distance matrix, using the given edge weight as a constant distance.
treeDistanceMatrix :: (a -> Double) -> BinaryTree a -> DistanceMatrix
treeDistanceMatrix f t =
  let indexedTree = indexTreeInline t
      indices = inOrder $ fmap snd indexedTree --TODO could just use size to calculate this.
      pairs = parentChildPairs indexedTree
      distances = map (\ ((a0, i0), (a1, i1)) -> (f a0, (i0, i1))) pairs
   in createAdditiveDistanceMatrixFromDistances indices distances

constantWeightTreeDistanceMatrix :: Double -> BinaryTree a -> DistanceMatrix
constantWeightTreeDistanceMatrix edgeWeight = treeDistanceMatrix (const edgeWeight)

weightedTreeDistanceMatrix :: WeightedBinaryTree a -> DistanceMatrix
weightedTreeDistanceMatrix = treeDistanceMatrix snd


--Graphs
treeGraph :: BinaryTree (a, Int) -> Data.Graph.Graph
treeGraph Empty = Data.Graph.buildG (0, 0) []
treeGraph n@(Node (_, i) c0 c1) =
  let fromParent i Empty = []
      fromParent i (Node (_, i') c0 c1) =
        let ledge = fromParent i' c0
            redge = fromParent i' c1
            cedge = (i, i')
         in ledge ++ ((:) cedge redge)
      edges = (fromParent i c0) ++ (fromParent i c1)
      vertexNumbers = map snd (inOrder n)
      bounds = (minimum vertexNumbers, maximum vertexNumbers)
   in Data.Graph.buildG bounds edges

