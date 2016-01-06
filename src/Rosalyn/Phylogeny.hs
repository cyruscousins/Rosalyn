module Rosalyn.Phylogeny where

import Rosalyn.Random
import Rosalyn.Trees
import Rosalyn.Distance
import Rosalyn.Sequence
import Rosalyn.Sequencing

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
randomGenomePhylogeny :: Rand Genome -> Rand Int -> Rand (BinaryTree (Maybe Genome))
randomGenomePhylogeny genomeD phylogenySizeD = 
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

