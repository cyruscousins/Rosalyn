module Experiment.Experiment where

import Rosalyn.External.Spades
import Rosalyn.Random
import Rosalyn.Sequence
import Rosalyn.Sequencing
import Rosalyn.ListUtils

import Rosalyn.Trees
import Rosalyn.Executor

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

--A very poor pseudobayesian assembler.
--Procedure: Given a k to use (in kmerization), the number of iterations to run, a distribution over lengths, and a readset, pick random genomes, mutating as desired, searching for the one that is closest to containing each kmer an appropriate number of times.  Search by slight modification, biasing toward the correct size.  Distribution is assumed to have a unique maximum.

--Given the kmer profile and a length distribution, score an assembly (Genome).
--Lower scores represent better assemblies.
evaluateAssembly :: (Int, Multiset Kmer, Int) -> Rand Int -> Genome -> Rational
evaluateAssembly (k, readkmers, numKmers) lenD g =
  let lenG = length g
      plen = toRational $ prob lenD lenG
      assemblyKmers :: Multiset Kmer
      assemblyKmers = kmerizeReadSetMS k [g]
      numAssemblyKmers = (-) lenG (pred k)
      --Square error of differences between observed and expected frequencies, weighted by observed frequency.  Uses a prior that is not actually a distribution (pseudocount 1 with no denominator normalization).
      err :: Kmer -> Int -> Rational
      err k c = (-) ((1 + (fromIntegral (Data.Maybe.fromMaybe 0 (Data.Map.lookup k readkmers)))) % (fromIntegral numKmers)) ((fromIntegral c) % (fromIntegral numAssemblyKmers))
      weightedSSE = Data.Map.foldlWithKey (\ res key val -> (+) ((*) ((err key val) ^ 2) ((fromIntegral val) % (fromIntegral numAssemblyKmers))) res) (0 % 1) assemblyKmers
   in weightedSSE / plen

argmin :: (Ord o) => (a -> o) -> [a] -> a
argmin f l =
  let argmin' [a]   = (a, f a)
      argmin' (a:l) =
        let (a0, m0) = argmin' l
            m1 = f a
         in if ((<) m1 m0)
            then (a, m1)
            else (a0, m0)
      (a, m) = argmin' l
   in a

assemblePB'' :: (Int, Multiset Kmer, Int) -> (Genome, Rational) -> Int -> Rand Int -> Rand Genome
assemblePB'' _ (g, _) 0 _ = Return g
assemblePB'' kmers (g0, s0) count lenD =
  do g1 <- mutateRead (mutateP 0.25) g0
     let s1 = evaluateAssembly kmers lenD g1
      in assemblePB'' kmers (if (<) s1 s0 then (g1, s1) else (g0, s0)) (pred count) lenD

assemblePB' :: (Int, Multiset Kmer, Int) -> Int -> Int -> Rand Int -> Rand Genome
assemblePB' kmers count countPer lenD =
  do seeds <- mapM (const (randomGenome lenD)) [1..count]
     genomes <- mapM (\x -> assemblePB'' kmers (x, evaluateAssembly kmers lenD x) countPer lenD) seeds
     return (argmin (evaluateAssembly kmers lenD) genomes)
     
assemblePB :: Int -> Int -> Rand Int -> ReadSet -> Rand Genome
assemblePB k count len reads =
  let kmers = kmerizeReadSetMS k reads
      totalKmers = sizeMultiset kmers
   in assemblePB' (k, kmers, totalKmers) count count len

assemblyExperiment :: Int -> Int -> Rand (Genome, Genome, Ratio Int)
assemblyExperiment k iterations =
--  let gLenD = UniformEnum (100, 120)
--      rLenD = UniformEnum (10, 40)
--      rCountD = UniformEnum (50, 70)
  let gLenD = UniformEnum (50, 60)
      rLenD = UniformEnum (10, 20)
      rCountD = UniformEnum (20, 30)
   in do s0 <- randomGenome (gLenD)
         r <- sequenceGenomeReads s0 rCountD rLenD
         s1 <- assemblePB k iterations gLenD r
         return (s0, s1, readsetDistance k [s0] [s1])

--Takes genome length distribution, read length distribution, section count, read per section distribution, section length distribution, read mutation function, and chimer frequency.
spadesExperiment :: Rand Int -> Rand Int -> Int -> Rand Int -> Rand Int -> (String -> Rand String) -> Rand Bool -> Rand (Genome, ReadSet, [(String, String)])
spadesExperiment gLenD rLenD sections readsPerSectionD sectionLenD mutator chmD =
  do --g <- randomGenome (gLenD)
     len <- gLenD --Length of the genome
     hmm <- (dnaHMM 0.0 256) --Random HMM to generate the genome
     (h, g) <- liftM unzip (hmmRand hmm 0 len) --The genome generated from the HMM
     r <- sequenceGenomeReadsBiased g sections readsPerSectionD sectionLenD rLenD mutator chmD
     return (g, r, spadesUnsafe (map (\ s -> s) r))

spadesExperimentDefault :: Rand (Genome, ReadSet, [(String, String)])
spadesExperimentDefault =
  --Spades is pretty picky, and doesn't like small data sets.
  let gLenD = UniformEnum (1500, 3000)
      rLenD = UniformEnum (75, 100)
      sections = 64
      rCountD = UniformEnum (1, 25)
      regionLenD = UniformEnum (200, 2000)
      mutator = mutateRead $ mutateP 0.1 -- 1 / 10 probability of uniform mutation
      chmD = Flip 0.1 -- 1 / 10 probability of chimerization.
   in spadesExperiment gLenD rLenD sections rCountD regionLenD mutator chmD

runSpadesExperimentDefault :: ((Genome, Int), ReadSet, [(String, String, Int)], Int)
runSpadesExperimentDefault =
  let ((g, r, a), _) = (sample spadesExperimentDefault gen0)
      gl = length g
      evaluations = map (\ (_, y) -> (evaluateAssemblyLD g y)) a
   in ((g, gl), r, map (\ ((a, b), c) -> (a, b, c)) (zip a evaluations), minimum evaluations)

--TODO parameter for mixing degree.
--This second experiment generates a genome and a 
--Takes genome length distribution, read length distribution, section count, read per section distribution, section length distribution, read mutation function, and chimer frequency.
spadesExperiment2 :: Rand Int -> Rand Int -> Int -> Rand Int -> Rand Int -> (String -> Rand String) -> Rand Bool -> Rand (((Genome, Int, Ratio Int), (Genome, Int, Ratio Int), Int), (ReadSet, ReadSet), (([(String, String)], [(String, String)]), ([(String, String)], [(String, String)])), ((Int, Int), (Int, Int)))
spadesExperiment2 gLenD rLenD sections readsPerSectionD sectionLenD mutator chmD =
  do --g <- randomGenome (gLenD)
     len <- gLenD --Length of the genome
     hmm <- (dnaHMM 0.0 256) --Random HMM to generate the genome
     (h, g0) <- liftM unzip (hmmRand hmm 0 len) --The genome generated from the HMM
     g1 <- mutateGenomeIterated 5 g0
     r0 <- sequenceGenomeReadsBiased g0 sections readsPerSectionD sectionLenD rLenD mutator chmD
     r1 <- sequenceGenomeReadsBiased g1 sections readsPerSectionD sectionLenD rLenD mutator chmD
     let a0 = spadesUnsafe r0
         a1 = spadesUnsafe r1
         --TODO selection should be random.
         --Selection should be a small fraction, so it could easily be random if it doesn't support existing information.  Should shoot for coverage 1?
         a0' = spadesUnsafe (r0 ++ (take (div (length r1) 10) r1))
         a1' = spadesUnsafe (r1 ++ (take (div (length r0) 10) r0))
         aToEval :: Genome -> [(String, String)] -> Int
         aToEval g l = evaluateAssemblyContigsLD g (map snd l)
      in return (((g0, length g0, readDepth g0 r0), (g1, length g1, readDepth g1 r1), levenshteinDistance defaultEditCosts g0 g1), (r0, r1), ((a0, a1), (a0', a1')), ((aToEval g0 a0, aToEval g1 a1), (aToEval g0 a0', aToEval g1 a1')))

--TODO this experiment didn't go so well.  It might be better if we gave spades the original dataset as contigs.
spadesExperiment2Default =
  --Spades is pretty picky, and doesn't like small data sets.
  let gLenD = UniformEnum (2000, 3000)
      rLenD = UniformEnum (75, 100)
      sections = 64
      rCountD = UniformEnum (1, 10)
      regionLenD = UniformEnum (200, 2000)
      mutator = mutateRead $ mutateP 0.1 -- 1 / 10 probability of uniform mutation
      chmD = Flip 0.1 -- 1 / 10 probability of chimerization.
   in spadesExperiment2 gLenD rLenD sections rCountD regionLenD mutator chmD

--Given 2 readsets and an evaluation function, split the readsets and determine the intra and inter readset distances (respectively).
evaluateReadsetDistanceMetricSubset :: ReadSet -> ReadSet -> (ReadSet -> ReadSet -> Prob) -> Rand ([Prob], [Prob])
evaluateReadsetDistanceMetricSubset r0 r1 d =
  do (r00, r01) <- randomSplit r0
     (r10, r11) <- randomSplit r1
     return (evaluateReadsetDistanceMetric [[r00, r01], [r10, r11]] d)
     --return ([d r00 r01, d r10 r11], [d r00 r10, d r00 r11, d r01 r10, d r01 r11])

evaluateReadsetDistanceMetric :: [[ReadSet]] -> (ReadSet -> ReadSet -> Prob) -> ([Prob], [Prob])
evaluateReadsetDistanceMetric r d =
  let i@(intra, inter) = intraInterUnorderedPairs r
   in mapT2 (map (uncurry d)) i

--symmetric KL (with pseudocount 1) divergence
klReadsetDistance :: Int -> ReadSet -> ReadSet -> Double
klReadsetDistance k a b =
  let (ak, bk) = mapT2 (kmerizeReadSet k) (a, b) --List of kmers
      (aks, bks) = mapT2 lexicographicSort (ak, bk) --Sorted lists of kmers.
      al = allListsSorted k nucleotides --All possible kmers (in sorted order).
      (aks', bks') = mapT2 (merge al) (aks, bks) --Sorted lists of kmers with pseudocount 1.
      ar = Rosalyn.Random.fromSortedList (aks')
      br = Rosalyn.Random.fromSortedList (bks')
      dist = symKlDiscrete ar br
   in sqrt $ Data.Maybe.fromJust dist --TODO is this actually a metric?
   --in if Data.Maybe.isNothing dist then -1 else Data.Maybe.fromJust dist

--Jensen-Shannon distance
jsReadsetDistance :: Int -> ReadSet -> ReadSet -> Double
jsReadsetDistance k a b =
  let ak = kmerizeReadSet k a
      bk = kmerizeReadSet k b
      ar = Rosalyn.Random.fromList ak
      br = Rosalyn.Random.fromList bk
   in sqrt $ jsDiscrete ar br

--Subramanian & Shwartz metric

--TODO S&S metric.

--This experiment is designed to evaluate readset distance metrics.
--It works by generating a readset for a genome and a closely related genome, sequencing reads for each, splitting the readset into a random partition, and evaluating the intra and inter species readset distances.  The AUROC is then taken between the intra and inter readset distances.  A good metric will have lower intraspecies readset distances than interspecies readset distances, and thus an AUROC near 1. 
--TODO parameterize to take original genomes and sequencing parameters.
--TODO add read correction: SPADES BayesHammer should work.
readsetEvaluationExperiment = 
  let gLenD = UniformEnum (100, 300)
      rLenD = UniformEnum (5, 50)
      sections = 32
      readsPerSectionD = UniformEnum (3, 5)
      sectionLenD = UniformEnum (200, 2000)
      mutator = mutateRead $ mutateP 0.1 -- 1 / 10 probability of uniform mutation
      chmD = Flip 0.1 -- 1 / 10 probability of chimerization.
   in do len <- gLenD --Length of the genome
         hmm <- (dnaHMM 0.0 256) --Random HMM to generate the genome
         (h, g0) <- liftM unzip (hmmRand hmm 0 len) --The genome generated from the HMM
         g1 <- mutateGenomeIterated 5 g0
         r0 <- sequenceGenomeReadsBiased g0 sections readsPerSectionD sectionLenD rLenD mutator chmD
         r1 <- sequenceGenomeReadsBiased g1 sections readsPerSectionD sectionLenD rLenD mutator chmD
         --evaluation <- mapM (const $ (evaluateReadsetDistanceMetric r0 r1 (\ a b -> realToFrac $ readsetDistance 8 a b))) [0..10]
         evaluation <- mapM (const $ (evaluateReadsetDistanceMetricSubset r0 r1 (\ a b -> realToFrac $ jsReadsetDistance 3 a b))) [0..50]
         let (intra, inter) = (concat $ map fst evaluation, concat $ map snd evaluation)
             --(intraF, interF) = (filter ((<=) 0) intra, filter ((<=) 0) inter)
          in return (evaluation, auroc intra inter)

--Given initial genome, size of tree, and mutations per construct a random phylogeny tree.
randomPhylogeny :: Genome -> Int -> Int -> Rand (BinaryTree Genome)
randomPhylogeny g0 tSize mCount =
  do treeShape <- sizedRandom tSize
     descentWithModification treeShape g0 (mutateGenomeIterated mCount)

