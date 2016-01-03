{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Rosalyn.Sequencing where

import GHC.Generics (Generic)

import Rosalyn.Random
import Rosalyn.Trees
import Rosalyn.Executor
import Rosalyn.ListUtils

import System.Random

import Data.List
import Data.Ratio
import Data.Char
import Data.Hashable
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Graph

import Control.Monad

import Text.EditDistance

import Numeric


import Debug.Trace
import System.IO.Unsafe


type Nucleotide = Char
type Sequence = [Nucleotide]

type Genome = Sequence
type SRead = Sequence

type ReadSet = [SRead]
type List a = [a]

--Basic nucleotide operations

complement :: Nucleotide -> Nucleotide
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'

reverseComplement :: [Char] -> [Char]
reverseComplement = (map complement) . reverse

nucleotides :: [Nucleotide]
nucleotides = ['A', 'T', 'C', 'G']

randomNucleotide :: Rand Char
randomNucleotide = Uniform nucleotides

randomGenomeLen :: Int -> Rand Genome
randomGenomeLen 0 = Return ""
randomGenomeLen i =
  do n <- randomNucleotide
     g <- randomGenomeLen (pred i)
     return ((:) n g)

randomGenome :: Rand Int -> Rand Genome
randomGenome lenD =
  do len <- lenD
     randomGenomeLen len

-----------
--Quality--
--Quality:

clamp :: (Ord a) => a -> a -> a -> a
clamp min val max
  | (min > val) = min
  | (max < val) = max
  | otherwise = val

--An integral Phred score, used in FASTQ and other data interchange.
--TODO need to refactor: Solexa/Illumina scores aren't actually Phred scores.
--Phred: Q = -10 log10 E
--Solexa/Illumina: Q = -10 log10 (E / (1-E))
data PhredFormat = Sanger | SolexaIllumina deriving (Eq, Generic, Hashable)

phredChr0 :: PhredFormat -> Char
phredChr0 Sanger = chr 33
phredChr0 SolexaIllumina = chr (59 + 5)

phredMinQ :: PhredFormat -> Int
phredMinQ Sanger = 0
phredMinQ SolexaIllumina = -5

phredMaxQ :: PhredFormat -> Int
phredMaxQ Sanger = 93
phredMaxQ SolexaIllumina = 62

--Phred quality scores.
data Phred = Phred PhredFormat Int deriving (Eq, Generic, Hashable)
instance Show Phred where
  show (Phred fmt val) = [chr ((ord $ phredChr0 fmt) + (clamp (phredMinQ fmt) val (phredMaxQ fmt)))]

--Continuous Phred score.  Used to convert between probabilities and integral Phred scores.  This representation is nearly lossless as compared to a floating point value, as it is essentially a scaled log probability with an odd base.
data PhredF = PhredF PhredFormat Prob deriving (Eq, Generic, Hashable)
instance Show PhredF where
  show = show . phredFToPhred

--TODO: These are wrong for Solexa/Illumina reads, especially for large error probabilities.

--TODO rounding to the nearest probability is not the same as rounding to the nearest log probability.  I believe we need to round down after adding log_10 (50) - log_10 (10) ~ .69897.
phredFToPhred :: PhredF -> Phred
phredFToPhred (PhredF fmt v)
  | v < 0 = undefined
  | v == 1.0 / 0.0 = Phred fmt 1000 --TODO this is totally arbitrary (but represents a very low probability).  This is done because of a strange floor implementation taking infinity onto 0.
  | otherwise = Phred fmt (floor (v + 0.69897))

probToPhredF :: PhredFormat -> Prob -> PhredF
probToPhredF fmt p
  | (p < 0) || (p > 1) = undefined
  | otherwise = PhredF fmt (-10 * (logBase 10 p))

probToPhred :: PhredFormat -> Prob -> Phred
probToPhred fmt = phredFToPhred . (probToPhredF fmt)

phredToPhredF :: Phred -> PhredF
phredToPhredF (Phred fmt v) = PhredF fmt (fromIntegral v)

phredFToProb :: PhredF -> Prob
phredFToProb (PhredF fmt v) = 10 ** (-v / 10)

phredToProb :: Phred -> Prob
phredToProb = phredFToProb . phredToPhredF


--Basic sequencing function.  Produces a read distributed according to the given distribution.
sequenceGenomeRead :: Genome -> Rand Int -> Rand SRead
sequenceGenomeRead g lenD =
  do rl <- Condition lenD ((>=) (length g))
     r0 <- UniformEnum (1, (-) (length g) (succ rl))
     return $ ((take rl) . (drop r0)) g

sequenceGenomeReads :: Genome -> Rand Int -> Rand Int -> Rand ReadSet
sequenceGenomeReads g numD lenD = 
  do num <- numD
     mapM (const (sequenceGenomeRead g lenD)) [1..num]

mutateP :: Prob -> Nucleotide -> Rand Nucleotide
mutateP p n = Bind (coinFlip p) (\x -> if x then Return n else randomNucleotide)

mutateRead :: (Nucleotide -> Rand Nucleotide) -> SRead -> Rand SRead
mutateRead = mapM

introduceReadError :: (Nucleotide -> Rand (Nucleotide, Prob)) -> SRead -> Rand [(Nucleotide, Prob)]
introduceReadError = mapM

--                          Length Number Entropy
sequenceGenome :: Genome -> Int -> Int -> StdGen -> ReadSet
sequenceGenome _ _ 0 _ = []
sequenceGenome g l c g0 =
  let (s, g1) = (randomR (0, (-) (length g) l) g0) :: (Int, StdGen)
      (f, g2) = (random g1) :: (Bool, StdGen)
      ss = (take l) . (drop s) $ g
      ss' = if f then reverse ss else ss
   in (:) ss' (sequenceGenome g l (pred c) g2)

--Trisect a list into ([0, p), [p, p + c), [p + c, length)) 
trisectList :: Int -> Int -> [a] -> ([a], [a], [a]) 
trisectList p c l0 =
  let (s0, l1) = splitAt p l0
      (s1, s2) = splitAt c l1
   in (s0, s1, s2)

--Types of sequence transformation
inversion :: Genome -> (Int, Int) -> Genome
inversion s (i, c) =
  let (s0, s1, s2) = trisectList i c s
   in s0 ++ (reverse s1) ++ s2
randomInversion :: Genome -> Rand Genome
randomInversion g =
  do i <- Uniform [0..(pred (length g))]
     c <- Uniform [0..((-) (pred (length g)) i)]
     return (inversion g (i, c))


deletion :: Genome -> (Int, Int) -> Genome
deletion s (i, c) = 
  let (s0, _, s2) = trisectList i c s
   in s0 ++ s2
randomDeletion :: Genome -> Rand Genome
randomDeletion g =
  do i <- Uniform [0..(pred (length g))]
     c <- Uniform [0..((-) (pred (length g)) i)]
     return (deletion g (i, c))


duplication :: Genome -> (Int, Int, Int) -> Genome
duplication s (i0, c, i1) =
  let (_, s1, _) = trisectList i0 c s
      (s0, s2) = splitAt i1 s
   in s0 ++ s1 ++ s2
randomDuplication :: Genome -> Rand Genome
randomDuplication g =
  do i0 <- Uniform [0..(pred (length g))]
     c  <- Uniform [0..((-) (pred (length g)) i0)]
     i1 <- Uniform [0..(pred (length g))]
     return (duplication g (i0, c, i1))

{-
mutation (i0, m) =
  let (s0, s1) = splitAt i0 s
   in s0 ++ ((:) m (tail s1))
randomMutation =
  do i <- Uniform [0..(pred gl)]
     c <- randomNucleotide
     return (mutation (i, c))
-}

mutateGenome :: Genome -> Rand Genome
mutateGenome g =
  do action <- Uniform [randomDeletion, randomInversion, randomDuplication, mutateRead (mutateP (1.0 / 100.0))]
     action g

mutateGenomeIterated :: Int -> Genome -> Rand Genome
mutateGenomeIterated 0 g = Return g
mutateGenomeIterated x g =
  do g2 <- mutateGenomeIterated (pred x) g
     mutateGenome g2

assemble :: Genome -> Int -> Int -> StdGen -> ReadSet
assemble = undefined

genomeDistance :: Genome -> Genome -> Int
genomeDistance = undefined


--Kmers
--TODO use a proper multiset implementation.
--type Multiset a = (Ord a) => Data.Map.Map a Int
--TODO type constraints?
type Multiset a = Data.Map.Map a Int
type Kmer = String

insertMultiset :: (Ord a) => a -> Multiset a -> Multiset a
insertMultiset a m = Data.Map.alter (\x -> Just $ maybe 1 succ x) a m

intersectionMultiset :: (Ord a) => Multiset a -> Multiset a -> Multiset a
intersectionMultiset = Data.Map.intersectionWith min

unionMultiset :: (Ord a) => Multiset a -> Multiset a -> Multiset a
unionMultiset = Data.Map.intersectionWith (+)

sizeMultiset :: (Ord a) => Multiset a -> Int
sizeMultiset = Data.Map.fold (+) 0

--Generalized Jacard Similarity.
--TODO not a good metric for kmers similarity because of read depth differences.
generalizedJacardMultiset :: (Ord a) => Multiset a -> Multiset a -> Ratio Int
generalizedJacardMultiset a b =
  let mins  = Data.Map.intersectionWith min a b
      maxes = Data.Map.unionWith max a b
      num   = Data.Map.fold (+) 0 mins
      denom = Data.Map.fold (+) 0 maxes
   in (%) num denom
{-
  let c Nothing Nothing = undefined
      c (Just a) Nothing = Just (a, 0)
      c (Just a) (Just b) = Just (max a b, min a b)
      c Nothing (Just a) = Just (0, a)
      pairs = Data.Map.unionWith c a b
      numerator = sum (map snd) pairs
      denominator = sum (map fst) pairs
   in (%) numerator denominator
-}

kmerizeRead :: Int -> SRead -> [Kmer]
kmerizeRead i r = map (take i) (take (succ ((-) (length r) i)) $ tails r)

kmerizeReadSet :: Int -> ReadSet -> [Kmer]
kmerizeReadSet k reads = concat $ map (kmerizeRead k) reads

--TODO heavily optimizable.
kmerizeReadSetMS :: Int -> ReadSet -> Multiset Kmer
kmerizeReadSetMS k reads =
  let kmers = kmerizeReadSet k reads
   in foldr insertMultiset Data.Map.empty kmers

readsetDistance :: Int -> ReadSet -> ReadSet -> Ratio Int
readsetDistance k a b =
  let rs0 = kmerizeReadSetMS k a
      rs1 = kmerizeReadSetMS k b
   in generalizedJacardMultiset rs0 rs1

kmersetDistanceMatrix :: [Multiset Kmer] -> (Int -> Int -> Ratio Int)
kmersetDistanceMatrix items =
  let l = length items
      pairs :: [((Int, Multiset Kmer), (Int, Multiset Kmer))]
      pairs = allPairsUnorderedIndexed items
      distances :: [((Int, Int), Ratio Int)]
      distances = map (\ ((i0, m0), (i1, m1)) -> ((i0, i1), generalizedJacardMultiset m0 m1)) pairs
      dmap :: Data.Map.Map (Int, Int) (Ratio Int)
      dmap = Data.Map.fromList distances
      f a b
        | (==) a b = 0
        | (<) b a = f b a
        | otherwise = Data.Maybe.fromJust $ Data.Map.lookup (a, b) dmap
   in f

readSetDistanceMatrix :: Int -> [ReadSet] -> (Int -> Int -> Ratio Int)
readSetDistanceMatrix i r = kmersetDistanceMatrix $ map (kmerizeReadSetMS i) r

descentWithModification :: BinaryTree () -> a -> (a -> Rand a) -> Rand (BinaryTree a)
descentWithModification Empty _ _ = Return Empty
descentWithModification (Node () c0 c1) v f =
  do v0 <- f v
     v1 <- f v
     c0' <- descentWithModification c0 v0 f
     c1' <- descentWithModification c1 v1 f
     return $ Node v c0' c1' ;

--Distribution over initial genome sizes.
--Distribution over phylogeny sizes.
randomGenomePhylogeny :: Rand Int -> Rand Int -> Rand (BinaryTree (Maybe Genome))
randomGenomePhylogeny genomeSize phylogenySize = 
  do treeStructure <- randomTreeShape phylogenySize
     g0 <- randomGenome genomeSize
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


--Sequence with reverse complementing, mutations, and chimers.
sequenceGenomeReadAdvanced :: Genome -> (Rand Int) -> (String -> Rand String) -> (Rand Bool) -> (Rand SRead)
sequenceGenomeReadAdvanced g0 lenD m chmD =
  do seq <- sequenceGenomeRead g0 lenD
     seq' <- m seq
     reverse <- (Flip 0.5)
     isChimer <- chmD
     let reversed   = if reverse  then reverseComplement seq' else seq
         chimerized = if isChimer then (fmap ((++) reversed) (sequenceGenomeReadAdvanced g0 lenD m chmD)) else return reversed
      in chimerized

--Randomly select regions to sample from, then sample from said regions.  This introduces sampling bias.
--Genome, number of regions, samples per region, size of region, size of samples, mutator, chimer frequency
sequenceGenomeReadsBiased :: Genome -> Int -> (Rand Int) -> (Rand Int) -> (Rand Int) -> (String -> Rand String) -> (Rand Bool) -> (Rand [SRead])
sequenceGenomeReadsBiased _ 0 _ _ _ _ _ = Return []
sequenceGenomeReadsBiased g0 regions regionCountD regionLenD sampleLenD mutator chmD =
  do rLen <- regionLenD
     r0 <- (UniformEnum (0, (length g0) - rLen - 1))
     numSamples <- regionCountD
     samples <- mapM (const $ sequenceGenomeReadAdvanced ((\ (a, b, c) -> b) (trisectList r0 rLen g0)) sampleLenD mutator chmD) [1..numSamples]
     (liftM ((++) samples)) (sequenceGenomeReadsBiased g0 (pred regions) regionCountD regionLenD sampleLenD mutator chmD)

--Turn a single read into a paired read by removing the middle of it and reverse complementing one side of it.
readToPairedRead :: Rand Int -> SRead -> Rand (SRead, SRead)
readToPairedRead len r =
  do l0 <- len
     l1 <- len
     if (l0 + l1 > (length r))
     then readToPairedRead len r
     else return ((take l0 r), map complement ((take l1) (reverse r)))

--TODO this function is highly inefficient!  Mutation is applied to internal strings where it is promptly discarded.
sequenceGenomePairedReadsBiased :: Genome -> Int -> (Rand Int) -> (Rand Int) -> (Rand Int) -> (Rand Int) -> (String -> Rand String)-> (Rand Bool) -> (Rand [(SRead, SRead)])
sequenceGenomePairedReadsBiased g0 regions regionCountD regionLenD pairLenD readLenD mutator chmD =
  do fullReads <- sequenceGenomeReadsBiased g0 regions regionCountD regionLenD pairLenD mutator chmD
     mapM (readToPairedRead readLenD) fullReads
 
readDepth :: Genome -> ReadSet -> Ratio Int
readDepth g r = (%) (sum (map length r)) (length g)

evaluateAssemblyLD :: Genome -> Genome -> Int 
evaluateAssemblyLD a b = min (levenshteinDistance defaultEditCosts a b) (levenshteinDistance defaultEditCosts a (reverseComplement b))

--Gives the lowest LD between a contig and the genome.
--Not a great metric.
evaluateAssemblyContigsLD :: Genome -> [Genome] -> Int
evaluateAssemblyContigsLD g c = minimum (map (evaluateAssemblyLD g) c)

--An MM capable of producing complex patterns like those found in genomic DNA.
dnaHMM :: Prob -> Int -> (Rand (HiddenMarkovModel Int Nucleotide))
dnaHMM stayProb hiddenStates =
  do transitions <- mapM (const $ randomDistribution [0..(pred hiddenStates)]) [0..(pred hiddenStates)]
     emissions <- mapM (const $ randomDistribution nucleotides) [1..hiddenStates]
     return $ HiddenMarkovModel (MarkovModel (\s -> Bind (Flip stayProb) (\x -> if x then Return s else (!!) transitions s))) ((!!) emissions)

