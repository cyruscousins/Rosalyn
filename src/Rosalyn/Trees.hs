{-# LANGUAGE FlexibleInstances #-}
module Rosalyn.Trees where

import Rosalyn.Random

import Data.Maybe
import qualified Data.Graph

type List a = [a]

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Empty

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

instance Randomizable (BinaryTree ()) where
  sizedRandom size =
    do shuffle <- shuffleList [1..size]
       return (fmap (const ()) (bstFromList shuffle))


--TODO instance IsList

--In order traversal.
inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node v c0 c1) = (inOrder c0) ++ ((:) v (inOrder c1))

--TODO redundant wih listUtils.
indexList :: [a] -> [(a, Int)]
indexList l0 =
  let indexList' [] _ = []
      indexList' (a:l) i = (:) (a, i) (indexList' l (succ i))
   in indexList' l0 0

indexTree :: BinaryTree a -> [(a, Int)]
indexTree = (indexList . inOrder)

indexTreeNonempty :: BinaryTree (Maybe a) -> [(a, Int)]
indexTreeNonempty t =
  let ordered = inOrder t
      nonEmpty = map fromJust $ filter isJust ordered
      indexed = indexList nonEmpty
   in indexed

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

