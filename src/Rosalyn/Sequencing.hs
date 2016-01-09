{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Rosalyn.Sequencing where

import GHC.Exts

import Rosalyn.Sequence
import Rosalyn.Random
import Rosalyn.Trees
import Rosalyn.Executor
import Rosalyn.ListUtils

import System.Random

import Data.List
import Data.Ratio
import Data.Char
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Graph
import qualified Data.Set as Set

import Control.Monad hiding (mapM, sequence)
import Data.Traversable

import Text.EditDistance

import Numeric


import Debug.Trace
import System.IO.Unsafe

--TODO remove or rename these: they're not particularly useful.
randomGenomeLen :: Int -> Rand Genome
randomGenomeLen l =
  let replicated :: [Rand Nucleotide]
      replicated = (replicate l randomNucleotide)
      sequenced :: Rand [Nucleotide]
      sequenced = sequence replicated
      fl :: [Nucleotide] -> Genome
      fl = GHC.Exts.fromList
      packed :: Rand Genome
      packed = fmap fl sequenced
   in packed
--sequence $ fromList (replicate l randomNucleotide)
--mapM (const randomNucleotide) (emptySequence l) --TODO this wastes a ByteString.  Need a version of mapM that takes ranges.

{-
randomGenomeLen 0 = Return "" --TODO is there a language extension that allows 
randomGenomeLen i =
  do n <- randomNucleotide
     g <- randomGenomeLen (pred i)
     return ((:) n g)
-}

randomGenome :: Rand Int -> Rand Genome
randomGenome lenD =
  do len <- lenD
     randomGenomeLen len

--Basic sequencing function.  Produces a read distributed according to the given distribution.
sequenceGenomeRead :: Genome -> Rand Int -> Rand SRead
sequenceGenomeRead g lenD =
  do rl <- Condition lenD ((>=) (length g))
     r0 <- UniformEnum (1, (-) (length g) (succ rl))
     return $ ((take rl) . (drop r0)) g

sequenceGenomeReads :: Genome -> Rand Int -> Rand Int -> Rand ReadSet
sequenceGenomeReads g numD lenD = 
  do num <- numD
     sequence (replicate num (sequenceGenomeRead g lenD))

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
  let (s, g1) = sample (UniformEnum (0, (length g) - l)) g0
      (f, g2) = (sample UniformBounded g1) :: (Bool, StdGen)
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

--TODO is their a combinator that does this:
--f :: (Monad m) => a -> Int -> (a -> m a) -> m a
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

--TODO heavily optimizable.
kmerizeSequenceSetMS :: Int -> ReadSet -> Multiset Kmer
kmerizeSequenceSetMS k reads =
  let kmers = kmerizeSequenceSet k reads
   in foldr insertMultiset Data.Map.empty kmers

readsetDistance :: Int -> ReadSet -> ReadSet -> Ratio Int
readsetDistance k a b =
  let rs0 = kmerizeSequenceSetMS k a
      rs1 = kmerizeSequenceSetMS k b
   in generalizedJacardMultiset rs0 rs1

--TODO use distance matrix module for this.
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
readSetDistanceMatrix i r = kmersetDistanceMatrix $ map (kmerizeSequenceSetMS i) r

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
sequenceGenomeReadsBiased :: Genome -> Int -> (Rand Int) -> (Rand Int) -> (Rand Int) -> (String -> Rand String) -> (Rand Bool) -> (Rand ReadSet)
sequenceGenomeReadsBiased _ 0 _ _ _ _ _ = Return []
sequenceGenomeReadsBiased g0 regions regionCountD regionLenD sampleLenD mutator chmD =
  do rLen <- regionLenD
     r0 <- (UniformEnum (0, (length g0) - rLen - 1))
     numSamples <- regionCountD
     samples <- sequence (replicate numSamples $ sequenceGenomeReadAdvanced ((\ (a, b, c) -> b) (trisectList r0 rLen g0)) sampleLenD mutator chmD)
     (liftM ((++) samples)) (sequenceGenomeReadsBiased g0 (pred regions) regionCountD regionLenD sampleLenD mutator chmD)

--The above function uses the following data.  It's heavily parameterized, so it's less confusing to use a record type.
--TODO rather than regionSize, it would be good to have a Rand (Int, Int) for sampling regions.  This could capture more diverse biases.
data BiasedSequencerSpec = BiasedSequencerSpec {
  regionCount :: Int,
  samplesPerRegion :: Rand Int,
  regionSize :: Rand Int,
  sampleLength :: Rand Int,
  mutator :: String -> Rand String,
  chimerProbability :: Prob
  }

--128 uniformly distributed regions, each between 1000 and 10000 nucleotides long, with between 1 and 10 samples taken from the region.  Reads are 50 to 500 bp long (this may be an unrealistically wide distribution, which is useful because it tests a strange edge case).  Uniform mutation probability 1/10, chimer probability (within a region) probability 1/10.
basicSequencerSpec = BiasedSequencerSpec { regionCount = 128, regionSize = UniformEnum (1000, 10000), samplesPerRegion = UniformEnum (1, 10), sampleLength = UniformEnum (50, 500), mutator = mutateRead $ mutateP 0.1, chimerProbability = 0.1 }

sequenceGenomeReadsBiased' :: BiasedSequencerSpec -> Genome -> (Rand ReadSet)
sequenceGenomeReadsBiased' (BiasedSequencerSpec { regionCount = regionCount, samplesPerRegion = samplesPerRegion, regionSize = regionSize, mutator = mutator, chimerProbability = chimerProbability }) g = --TODO is there a more concise way to pattern match this?
  let cp = Flip chimerProbability
   in sequenceGenomeReadsBiased g regionCount samplesPerRegion regionSize samplesPerRegion mutator cp 

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

--An HMM capable of producing complex patterns like those found in genomic DNA.
dnaHMM :: Prob -> Int -> (Rand (HiddenMarkovModel Int Nucleotide))
dnaHMM stayProb hiddenStates =
  do transitions <- sequence (replicate hiddenStates $ randomDistribution [0..(pred hiddenStates)])
     emissions <- sequence (replicate hiddenStates $ randomDistribution nucleotides)
     return $ HiddenMarkovModel (MarkovModel (\s -> Bind (Flip stayProb) (\x -> if x then Return s else (!!) transitions s))) ((!!) emissions)


--Assembly statistics:

--TODO: Does laziness benefit here?
jacardSimilarity :: (Ord a) => Set.Set a -> Set.Set a -> Ratio Int
jacardSimilarity a b = (%) (Set.size (Set.intersection a b)) (Set.size (Set.union a b))

jacardDistance :: (Ord a) => Set.Set a -> Set.Set a -> Ratio Int
jacardDistance a b = 1 - (jacardSimilarity a b)

jacardKmerAssemblyCoverage :: Int -> Genome -> ReadSet -> Ratio Int
jacardKmerAssemblyCoverage i a b = jacardDistance (Set.fromList $ kmerizeSequence i a) (Set.fromList $ kmerizeSequenceSet i b)

jacardSubkmerAssemblyCoverage :: Int -> Genome -> ReadSet -> Ratio Int
jacardSubkmerAssemblyCoverage i a b = jacardDistance (Set.fromList $ subkmerizeSequence i a) (Set.fromList $ subkmerizeSequenceSet i b)



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

