{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Rosalyn.Phylogeny where

import qualified Data.Tree
import qualified Data.Set

import Rosalyn.Random
import Rosalyn.Trees
import Rosalyn.Distance
import Rosalyn.Sequence
import Rosalyn.Sequencing
import Rosalyn.ListUtils

import Control.Monad

---------------------
--Phylogeny data type

--The Phylogeny type represents an unordered rooted weighted tree.
--Unorderedness is (ironically) preserved by keeping the elements in sorted order: thus only one tree is valid for this rooting (assuming no equal labels).
--This doesn't provide much in the way of efficiency, but makes comparison easier.

--A phylogeny is parameterized by 3 type variables:
-- a gives the type of the tree labels.
-- b gives the type of the data stored at each node.
-- c gives the type of the edge lengths.

--type Phylogeny a b c = forall a b c . (Ord a, b, Num c) => Data.Tree.Tree (a, b, c)

--type Phylogeny a b c = forall a b c . (Ord a, Num c) => Data.Tree.Tree (a, b, c)
--type Phylogeny a b c = forall a b c . Data.Tree.Tree (a, b, c)
type Phylogeny a b c = Data.Tree.Tree (a, b, c)

listRemove :: Int -> [a] -> (a, [a])
listRemove i l =
  let (pre, post) = splitAt i l
   in (head post, pre ++ (tail post))

insertSorted :: (Ord a) => a -> [a] -> [a]
insertSorted a [] = [a]
insertSorted a (b:c)
 | a < b = b : (insertSorted a c)
 | otherwise = b : a : c

--Antizip pairs all a with all b, except those at identical indices.  Assumes equal length lists.
antiZip :: [a] -> [b] -> [(a, b)]
antiZip (a:al) (b:bl) =
  let abl = map (\x -> (a, x)) bl
      bal = map (\x -> (x, b)) al
      recur = antiZip al bl
   in abl ++ bal ++ recur

--Used to change the root of a phylogeny tree.
{-
promoteChild :: Phylogeny a b c -> Int -> Phylogeny a b c
promoteChild (Data.Tree.Node (lp, dp, _) l) i =
  let (Data.Tree.Node (lc, dc, eab) cc, l') = listRemove i l
      p' = Data.Tree.Node (lc, dc, eab) bl, l' --New parent (old child)
      c' = Data.Tree.Node (lp dp --New child (old parent)
   in Data.Tree.Node (vb, 0) (insertSorted (Data.Tree.Node (va eab) l') bl)
-} --TODO

--TODO use pads for DOT conversions.
toDot :: (a -> String) -> (b -> String) -> (c -> String) -> Phylogeny a b c -> String -> String
toDot ac bc cc = undefined

fromDot :: (String -> a) -> (String -> b) -> (String -> c) -> String -> (String, Phylogeny a b c)
fromDot = undefined

--Convert a phylogeny into an edge list, where edges are defined as a ((child, parent) length) tuple.
toEdgeList :: forall a b c . (Ord a) => Phylogeny a b c -> [((a, a), c)]
toEdgeList (Data.Tree.Node (p, _, _) children) =
  let pcEdges :: [((a, a), c)]
      pcEdges = map (\ (Data.Tree.Node (c, _, d) _) -> ((c, p), d)) children
      subTreeEdges :: [[((a, a), c)]]
      subTreeEdges = map toEdgeList children
   in concat (pcEdges : subTreeEdges)

--To get all distances in a phylogeny, we take the parent's distance to all children in each subtree, and append distances between each pair of subtrees.
toDistances :: forall a b c . (Real c) => Phylogeny a b c -> [((a, a), c)]
toDistances t = undefined
{-
  let toDistances' :: Phylogeny a b c -> ([((a, a), c)], [((a, a), c)])
      toDistances' (Data.Tree.Node (p, _, c) children) =
        let --Full distance matrix for each child tree.  Distances from nodes to the subtree root come in the first list, where the root is the second label, and distances between nonroot nodes come in the second list.
            childrenIntraDistances :: [([((a, a), c)], [((a, a), c)])]
            childrenIntraDistances = map toDistances' children
            childrenIntraTops :: [[((a, a), c)]]
            childrenIntraTops = map fst childrenIntraDistances
            childrenIntraBottoms :: [[((a, a), c)]]
            childrenIntraBottoms = map snd childrenIntraDistances
            --Distances to the current root from each element of each child.
            toParentDistances :: [[((a, a), c)]]
            toParentDistances = zipWith ( \ (Data.Tree.Node _ e) cTops -> map (\((c, _), d) -> (c, p, d + e)) cTops ) children childrenIntraTops
            --Distances between child subtrees.
            pairedInterChildren :: [[(((a, a), c), ((a, a), c))]]
            pairedInterChildren = map cartesianProduct (antiZip toParentDistances toParentDistances)
            childrenInterDistances :: [[((a, a), c)]]
            childrenInterDistances = map (\ (((c0, _), d0), ((c1, _), d1)) -> ((c0, c1), (d0 + d1))) pairedInterChildren
            toParent :: [((a, a), c)]
            toParent = concat toParentDistances
            remainder :: [((a, a), c)]
            remainder = (concat . concat) [childrenIntraTops, childrenIntraBottoms, childrenInterDistances]
         in (toParentDistances, remainder)
   in (uncurry (++)) (toDistances' t)
-}

toDistanceMatrix :: forall a b c . (Real c) => Phylogeny Int b c -> DistanceMatrix
toDistanceMatrix p = undefined
{-
  let distances :: [((a, a), c)]
      distances = toDistances p
      distances' = map (\ (p, d) -> (p, realToFrac d)) distances --TODO convert p to indices.
   in createDistanceMatrixFromDistances distances'
-}

treeSize :: Phylogeny a b c -> Int
treeSize (Data.Tree.Node _ l) = 1 + (sum $ map treeSize l)

collectLabels :: (Ord a) => Phylogeny a b c -> Data.Set.Set a
collectLabels t = Data.Set.fromList (map (\ (a, _, _) -> a) (Data.Tree.flatten t))

partitions :: (Ord a) => Phylogeny a b c -> [(Data.Set.Set a, Data.Set.Set a)]
partitions t@(Data.Tree.Node (pa, _, _) children) = undefined
{-
  let childSets = map collectLabels children --A set for each subtree
      fullSet = collectLabels t --Data.Set.insert pa (foldl0 Data.Set.union childSets)
      childSetsC = map ((Data.Set.\\) fullSet) childSets --Complement of childSets.
      localPartitions = zip childSets childSetsC
      childPartitionSets = map partitions children
      childPartitionSets' = zipWith (\ (l, u) c -> (l, (Data.Set.union u c))) childPartitionSets childSetsC
   in localPartitions ++ childPartitionSets'
-}

--TODO move the Robinson Foulds logic to Statistics.hs
robinsonFoulds :: forall a b c . (Ord a) => Phylogeny a b c -> Phylogeny a b c -> Int
robinsonFoulds a b = undefined
{-
  let pa :: [(Data.Set.Set a, Data.Set.Set a)]
      pa = partitions a
      pb = partitions b
      i = Data.Set.intersection pa pb
   in (Data.Set.size pa) + (Data.Set.size pb) - 2 * (Data.Set.size i)
-}

--------------------
--Phylogeny Mutators

--Map over the phylogeny labels.
mapLabels :: (a -> aa) -> Phylogeny a b c -> Phylogeny aa b c
mapLabels f = fmap (\(a, b, c) -> (f a, b, c))

--Map over phylogeny data cells.
mapData :: (b -> bb) -> Phylogeny a b c -> Phylogeny a bb c
mapData f = fmap (\(a, b, c) -> (a, f b, c))

--Calculate the distance between adjacent phylogeny nodes.
recalculateDistances :: (Num d) => (b -> b -> d) -> Phylogeny a b c -> Phylogeny a b d
recalculateDistances f = recalculateDistances' (\ _ (_, b) (_, b') -> f b b')

--More generic version of recalculateDistances.
--Gives access to labels, data, and old distance.
recalculateDistances' :: (Num d) => (c -> (a, b) -> (a, b) -> d) -> Phylogeny a b c -> Phylogeny a b d
recalculateDistances' f (Data.Tree.Node (a, b, c) children) =
  let rd p (Data.Tree.Node (a, b, c) children) =
        let children' = map (rd (a, b)) children
            c' = f c p (a, b)
         in Data.Tree.Node (a, b, c') children'
      children' = map (rd (a, b)) children
      p' = Data.Tree.Node (a, b, 0) children'
   in p'

--Remove all cells that don't meet a predicate, preserving distance relationships.  If the root is removed, the result is a forest, if the result is the empty tree, [] is returned, otherwise a single element is returned.
filterPhylogeny :: (Num c) => (b -> Bool) -> Phylogeny a b c -> [Phylogeny a b c]
filterPhylogeny f = filterPhylogeny' (\ _ b -> f b)

--TODO suboptimal function: should reuse subtrees that remain unchanged.
filterPhylogeny' :: forall a b c . (Num c) => (a -> b -> Bool) -> Phylogeny a b c -> [Phylogeny a b c]
filterPhylogeny' f p = undefined
{-
  let filterPhylogeny'' (Data.Tree.Node (a, b, c) children)
       | (f a b) == False =
         let filteredChildren = concatMap filterPhylogeny'' children --Predicate failed: remove this node.
             increaseDistance :: Phylogeny a b c -> c -> Phylogeny a b c
             increaseDistance (Data.Tree.Node (a, b, c) children) c' = Data.Tree.Node (a, b, c + c') children
             distanceCorrected = map (increaseDistance c) filteredChildren --Add parent-grandparent distance to child, since parent is filtered.
          in distanceCorrected
       | otherwise = [Data.Tree.Node (a, b, c) (concatMap filterPhylogeny'' children)]
      zeroDistance :: Phylogeny a b c -> Phylogeny a b c
      zeroDistance (Data.Tree.Node (a, b, c) children) = Data.Tree.Node (a, b, 0) children
      filtered = filterPhylogeny'' p
      rootCorrected = map zeroDistance filtered
   in rootCorrected
-}

--Take structurally identical phylogenies with identical labelings, and combine their data fields.  This function is partial, and not defined for nonidentical phylogenies.
--Combination is left biased on labels.
zipPhylogenies :: (Eq a) => (c -> e -> f) -> Phylogeny a b c -> Phylogeny a d e -> Phylogeny a (b, d) f
zipPhylogenies f (Data.Tree.Node (a, b, c) children) (Data.Tree.Node (a2, d, e) children2)
 | (a == a2) && (length children == length children2) = Data.Tree.Node (a, (b, d), f c e) (zipWith (zipPhylogenies f) children children2)

--Preorder index a phylogeny.
indexPhylogeny :: forall a b c e . (Enum e) => e -> Phylogeny a b c -> Phylogeny e b c
indexPhylogeny e0 p =
  let indexPhylogeny' :: e -> Phylogeny a b c -> (Phylogeny e b c, e)
      indexPhylogeny' e0 (Data.Tree.Node (_, b, c) children) =
        let e1 = succ e0
            folder :: Phylogeny a b c -> ([Phylogeny e b c], e) -> ([Phylogeny e b c], e)
            folder p (l, e) =
              let (p', e') = indexPhylogeny' e p
               in (p' : l, e')
            (children', e') = foldr folder ([], e1) children
         in (Data.Tree.Node (e0, b, c) children', e')
      (indexed, _) = indexPhylogeny' e0 p
   in indexed

------------------------------
--Advanced phylogeny operators

randomPhylogenyShape :: Int -> Rand Int -> Rand (Phylogeny () () Int)
randomPhylogenyShape maxDepth childrenCountD =
  let childrenCountD' = (replicate maxDepth childrenCountD) ++ [Return 0]
   in randomPhylogenyShape' childrenCountD'

randomPhylogenyShape' :: [Rand Int] -> Rand (Phylogeny () () Int)
randomPhylogenyShape' d =
  let rps :: [Rand Int] -> Rand (Phylogeny () () Int)
      rps (cDist:cDistl) =
        do childCount <- cDist
           children <- replicateM childCount (rps cDistl)
           return (Data.Tree.Node ((), (), 1) children)
      fixTop :: Phylogeny () () Int -> Phylogeny () () Int
      fixTop (Data.Tree.Node _ c) = Data.Tree.Node ((), (), 0) c
   in fmap fixTop (rps d)



descentWithModification :: BinaryTree () -> a -> (a -> Rand a) -> Rand (BinaryTree a)
descentWithModification Empty _ _ = Return Empty
descentWithModification (Node () c0 c1) v f =
  do v0 <- f v ;
     v1 <- f v ;
     c0' <- descentWithModification c0 v0 f ;
     c1' <- descentWithModification c1 v1 f ;
     return $ Node v c0' c1' ;

--Distribution over initial genomes.
--Distribution over phylogeny sizes.
randomExtantPhylogeny :: Rand Genome -> Rand Int -> Rand (BinaryTree (Maybe Genome))
randomExtantPhylogeny genomeD phylogenySizeD = 
  do phylogenySize <- phylogenySizeD
     treeStructure <- sizedRandom phylogenySize
     g0 <- genomeD
     fmap (treeContract . dropInternal) (descentWithModification treeStructure g0 mutateGenome)

{-
--Rooted binary phylogeny with no internal nodes.
data Phylogeny a = Node (Phylogeny a) (Phylogeny a) | Leaf a deriving Show

neighborJoining :: (Num n) => (a -> a -> n) -> [a] -> Phylogeny a
neighborJoining = undefined

randomPhylogeny :: Genome -> Prob -> StdGen -> (Phylogeny Genome, StdGen)
randomPhylogeny g b 0 r0 =
  let (f, r1) = randomR (0 :: Prob, 1) r0
   in if ((<=) f b) 

  let (g1, r1) (g, r)
randomPhylogeny g i r0 =
  let im1 = pred i
      
      (c0, r1) = randomPhylogeny im1 r0
      (c1, r2) = 
-}

--Given initial genome, size of tree, and mutations per construct a random phylogeny tree.
--TODO number of mutations should be replaced with a random mutation function.
randomPhylogeny :: Genome -> Int -> Int -> Rand (BinaryTree Genome)
randomPhylogeny g0 tSize mCount =
  do treeShape <- sizedRandom tSize
     descentWithModification treeShape g0 (mutateGenomeIterated mCount)

