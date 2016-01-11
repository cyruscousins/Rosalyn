{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Rosalyn.Sequencing where

import GHC.Exts

import Rosalyn.Sequence
import Rosalyn.Random
import Rosalyn.Statistics
import Rosalyn.Trees
import Rosalyn.Executor
import Rosalyn.ListUtils
import Rosalyn.Distance

import System.Random

import Prelude hiding (length, head, last, null, tail, map, filter, concat, any, lookup, init, all, foldl, foldr, foldl1, foldr1, maximum, minimum, iterate, span, break, takeWhile, dropWhile, reverse, zip, zipWith, sequence, sequence_, mapM, mapM_, concatMap, and, or, sum, product, repeat, replicate, cycle, take, drop, splitAt, elem, notElem, unzip, lines, words, unlines, unwords)
import Data.ListLike --hiding (sequence, mapM)

import Data.List (intercalate)
import Data.Ratio
import Data.Char
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Graph
import qualified Data.MultiSet
import qualified Data.Set as Set

import Control.Monad hiding (mapM, sequence)
import Data.Traversable hiding (sequence, mapM, length)

import Text.EditDistance

import Numeric


import Debug.Trace
import System.IO.Unsafe


--Basic sequencing function.  Produces a read distributed according to the given distribution.
sequenceGenomeRead :: Genome -> Rand Int -> Rand SRead
sequenceGenomeRead g lenD =
  do si <- uniformSubinterval (length g) lenD
     return (takeSubinterval si g)

sequenceGenomeReads :: Genome -> Rand Int -> Rand Int -> Rand ReadSet
sequenceGenomeReads g numD lenD = 
  do num <- numD
     replicateM num (sequenceGenomeRead g lenD)

mutateP :: Prob -> Nucleotide -> Rand Nucleotide
mutateP p n = Bind (coinFlip p) (\x -> if x then Return n else randomNucleotide)

mutateRead :: (Nucleotide -> Rand Nucleotide) -> SRead -> Rand SRead
mutateRead f r = mapM f r
--mutateRead f r = mapM (fNucToNuc8 f) r

introduceReadError :: (Nucleotide -> Rand (Nucleotide, Prob)) -> SRead -> Rand (SRead, [Prob]) --TODO use a quality sequence.
introduceReadError = undefined --TODO

--                          Length Number Entropy
sequenceGenome :: Genome -> Int -> Int -> StdGen -> ReadSet
sequenceGenome _ _ 0 _ = []
sequenceGenome g l c g0 =
  let (s, g1) = sample (UniformEnum (0, (length g) - l)) g0
      (f, g2) = (sample UniformBounded g1) :: (Bool, StdGen)
      ss = (take l) . (drop s) $ g
      ss' = if f then reverse ss else ss
   in (:) ss' (sequenceGenome g l (pred c) g2)

--Types of sequence transformation
inversion :: Genome -> (Int, Int) -> Genome
inversion s (i, c) =
  let (s0, s1, s2) = trisectList i c s
   in append s0 (append (reverseComplement s1) s2)
      --TODO concat is probably more efficient
      --concat [s0, (reverseComplement s1), s2]
randomInversion :: Genome -> Rand Genome
randomInversion g =
  do i <- Uniform [0..(pred (length g))]
     c <- Uniform [0..((-) (pred (length g)) i)]
     return (inversion g (i, c))

--Given true length and a distribution over subinterval lengths, produce a random subinterval (in (start, length) format).
uniformSubinterval :: Int -> Rand Int -> Rand (Int, Int)
uniformSubinterval iLen siLenD =
  do x <- siLenD --Condition siLenD ((>=) iLen) --TODO condition.
     s <- UniformEnum (0, iLen - x)
     Return (s, x)

takeSubinterval :: (Int, Int) -> Sequence -> Sequence
takeSubinterval (i0, l) = (take l) . (drop i0)

deletion :: Genome -> (Int, Int) -> Genome
deletion s (i, c) = 
  let (s0, _, s2) = trisectList i c s
   in append s0 s2
randomDeletionSized :: Rand Int -> Genome -> Rand Genome
randomDeletionSized s g =
  do interval <- uniformSubinterval (length g) s
     return (deletion g interval)
randomDeletion :: Genome -> Rand Genome
randomDeletion g = randomDeletionSized (UniformEnum (1, length g)) g

duplication :: Genome -> (Int, Int, Int) -> Genome
duplication s (i0, c, i1) =
  let (_, s1, _) = trisectList i0 c s
      (s0, s2) = splitAt i1 s
   in append s0 (append s1 s2)
      --TODO use concat
      --concat [s0, s1, s2]
randomDuplicationSized :: Rand Int -> Genome -> Rand Genome
randomDuplicationSized s g =
  do (s0, sLen) <- uniformSubinterval (length g) s
     d0 <- UniformEnum (0, length g)
     return $ duplication g (s0, sLen, d0)
randomDuplication :: Genome -> Rand Genome
randomDuplication g = randomDuplicationSized (UniformEnum (1, length g)) g

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

--TODO is there a library combinator that does this?
iterateM :: (Monad m) => (a -> m a) -> Int -> a -> m a
iterateM f 0 a = return a
iterateM f i a = 
  do a' <- f a
     iterateM f (pred i) (a')
 
mutateGenomeIterated :: Int -> Genome -> Rand Genome
mutateGenomeIterated i g = iterateM mutateGenome i g


--TODO heavily optimizable.
kmerizeSequenceSetMS :: Int -> ReadSet -> Data.MultiSet.MultiSet Kmer
kmerizeSequenceSetMS k reads =
  let kmers = kmerizeSequenceSet k reads
   in Data.MultiSet.fromList kmers

readsetDistance :: Int -> ReadSet -> ReadSet -> Ratio Int
readsetDistance k a b =
  let rs0 = kmerizeSequenceSetMS k a
      rs1 = kmerizeSequenceSetMS k b
   in generalizedJacardDistance rs0 rs1

--TODO need a Rational distance matrix.  Here we wrap one using floating points, possibly losing a bit of precision.
readSetDistanceMatrix :: Int -> [ReadSet] -> (Int -> Int -> (Ratio Int))
readSetDistanceMatrix i r =
  let dm = createDistanceMatrix (map (kmerizeSequenceSetMS i) r) (\ a b -> realToFrac $ generalizedJacardDistance a b)
      converted a b = realToFrac $ distance dm a b
   in converted

--Sequence with reverse complementing, mutations, and chimers.
sequenceGenomeReadAdvanced :: Genome -> (Rand Int) -> (Sequence -> Rand Sequence) -> (Rand Bool) -> (Rand SRead)
sequenceGenomeReadAdvanced g0 lenD m chmD =
  do seq <- sequenceGenomeRead g0 lenD
     seq' <- m seq
     reverse <- (Flip 0.5)
     isChimer <- chmD
     let reversed   = if reverse  then reverseComplement seq' else seq
         chimerized = if isChimer then (fmap ((append) reversed) (sequenceGenomeReadAdvanced g0 lenD m chmD)) else return reversed
      in chimerized

--Randomly select regions to sample from, then sample from said regions.  This introduces sampling bias.
--Genome, number of regions, samples per region, size of region, size of samples, mutator, chimer frequency
sequenceGenomeReadsBiased :: Genome -> Int -> (Rand Int) -> (Rand Int) -> (Rand Int) -> (Sequence -> Rand Sequence) -> (Rand Bool) -> (Rand ReadSet)
sequenceGenomeReadsBiased _ 0 _ _ _ _ _ = Return []
sequenceGenomeReadsBiased g0 regions regionCountD regionLenD sampleLenD mutator chmD =
  do rLen <- regionLenD
     r0 <- (UniformEnum (0, (length g0) - rLen - 1))
     numSamples <- regionCountD
     samples <- (replicateM numSamples $ sequenceGenomeReadAdvanced ((\ (a, b, c) -> b) (trisectList r0 rLen g0)) sampleLenD mutator chmD)
     (liftM ((append) samples)) (sequenceGenomeReadsBiased g0 (pred regions) regionCountD regionLenD sampleLenD mutator chmD)

--The above function uses the following data.  It's heavily parameterized, so it's less confusing to use a record type.
--TODO rather than regionSize, it would be good to have a Rand (Int, Int) for sampling regions.  This could capture more diverse biases.
data BiasedSequencerSpec = BiasedSequencerSpec {
  regionCount :: Int,
  samplesPerRegion :: Rand Int,
  regionSize :: Rand Int,
  sampleLength :: Rand Int,
  mutator :: SRead -> Rand SRead,
  chimerProbability :: Prob
  }

--128 uniformly distributed regions, each between 1000 and 10000 nucleotides long, with between 1 and 10 samples taken from the region.  Reads are 50 to 500 bp long (this may be an unrealistically wide distribution, which is useful because it tests a strange edge case).  Uniform mutation probability 1/10, chimer probability (within a region) probability 1/10.
basicSequencerSpec = BiasedSequencerSpec { regionCount = 128, regionSize = UniformEnum (1000, 10000), samplesPerRegion = UniformEnum (1, 10), sampleLength = UniformEnum (50, 500), mutator = mutateRead $ mutateP 0.1, chimerProbability = 0.1 }

--This fuction provides a convenient wrapper for sequenceGenomeReadsBiased
sequenceGenomeReadsBiased' :: BiasedSequencerSpec -> Genome -> (Rand ReadSet)
sequenceGenomeReadsBiased' spec g = sequenceGenomeReadsBiased g (regionCount spec) (samplesPerRegion spec) (regionSize spec) (samplesPerRegion spec) (mutator spec) (Flip $ chimerProbability spec) 

--Turn a single read into a paired read by removing the middle of it and reverse complementing one side of it.
readToPairedRead :: Rand Int -> SRead -> Rand (SRead, SRead)
readToPairedRead len r =
  do l0 <- len
     l1 <- len
     if (l0 + l1 > (length r))
     then readToPairedRead len r
     else return ((take l0 r), map complement ((take l1) (reverse r)))

--TODO this function is highly inefficient!  Mutation is applied to internal strings where it is promptly discarded.
sequenceGenomePairedReadsBiased :: Genome -> Int -> (Rand Int) -> (Rand Int) -> (Rand Int) -> (Rand Int) -> (Sequence -> Rand Sequence)-> (Rand Bool) -> (Rand [(SRead, SRead)])
sequenceGenomePairedReadsBiased g0 regions regionCountD regionLenD pairLenD readLenD mutator chmD =
  do fullReads <- sequenceGenomeReadsBiased g0 regions regionCountD regionLenD pairLenD mutator chmD
     mapM (readToPairedRead readLenD) fullReads
 
readDepth :: Genome -> ReadSet -> Ratio Int
readDepth g r =
  let lengths :: [Int]
      lengths = map length r
      numerator :: Int
      numerator = sum $ lengths
      denominator = length g
   in numerator % denominator

---------------
--DNA Synthesis

--An HMM capable of producing complex patterns like those found in genomic DNA.
dnaHMM :: Prob -> Int -> (Rand (HiddenMarkovModel Int Nucleotide))
dnaHMM stayProb hiddenStates =
  do transitions <- (replicateM hiddenStates $ randomDistribution [0..(pred hiddenStates)])
     emissions <- (replicateM hiddenStates $ randomDistribution nucleotides)
     return $ HiddenMarkovModel (MarkovModel (\s -> Bind (Flip stayProb) (\x -> if x then Return s else (!!) transitions s))) ((!!) emissions)

dnaHmmSynthesize :: Double -> Int -> Int -> Rand Genome
dnaHmmSynthesize stayProb hidden len =
  do hmm <- (dnaHMM 0.0 256) --Random HMM to generate the genome
     (_, g0) <- liftM (unzip :: [(a, b)] -> ([a], [b])) (hmmRand hmm 0 len) --The genome generated from the HMM
     return g0

