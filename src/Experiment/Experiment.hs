module Experiment.Experiment where

import Rosalyn.External.Spades
import Rosalyn.Random
import Rosalyn.Statistics
import Rosalyn.Sequence
import Rosalyn.Sequencing
import Rosalyn.ListUtils
import Rosalyn.Phylogeny
import Rosalyn.Distance

import Rosalyn.Trees
import Rosalyn.Executor

import GHC.Exts

import System.Random

--import Prelude hiding (length, head, last, null, tail, map, filter, concat, any, lookup, init, all, foldl, foldr, foldl1, foldr1, maximum, minimum, iterate, span, break, takeWhile, dropWhile, reverse, zip, zipWith, sequence, sequence_, mapM, mapM_, concatMap, and, or, sum, product, repeat, replicate, cycle, take, drop, splitAt, elem, notElem, unzip, lines, words, unlines, unwords)
--import Data.ListLike hiding (fromList)
--import qualified Data.ListLike as LL (fromList)
import Data.List (intercalate)

import Data.Ratio
import Data.Char
import Data.Hashable
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Graph

import Control.Monad hiding (mapM)

import Text.EditDistance

import Numeric


--Takes genome length distribution, read length distribution, section count, read per section distribution, section length distribution, read mutation function, and chimer frequency.
spadesExperiment :: Rand Int -> Rand Int -> Int -> Rand Int -> Rand Int -> (String -> Rand String) -> Rand Bool -> Rand (Genome, ReadSet, [(String, String)])
spadesExperiment gLenD rLenD sections readsPerSectionD sectionLenD mutator chmD =
  do --g <- randomGenome (gLenD)
     len <- gLenD --Length of the genome
     hmm <- (dnaHMM 0.0 256) --Random HMM to generate the genome
     (h, g) <- liftM unzip (hmmRand hmm 0 len) --The genome generated from the HMM
     r <- sequenceGenomeReadsBiased g sections readsPerSectionD sectionLenD rLenD mutator chmD
     return (g, r, runProgramUnsafe Spades r)

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

runSpadesExperimentDefault :: ((Genome, Int), ReadSet, [(String, String, Ratio Int)], Ratio Int)
runSpadesExperimentDefault =
  let ((g, r, a), _) = (sample spadesExperimentDefault gen0)
      contigs :: [Sequence]
      contigs = map snd a
      gl = length g
      contigEvaluations = map (jacardSubkmerCoverage 100 g) contigs
      assemblyEvaluation = jacardSubkmerAssemblyCoverage 100 g contigs
   in ((g, gl), r, map (\ ((a, b), c) -> (a, b, c)) (zip a contigEvaluations), assemblyEvaluation)


--Takes genome length distribution, function to produce a mutated genome from an existing genome, read length distribution, section count, read per section distribution, section length distribution, read mutation function, chimer frequency, and mix ratio.
--Produces: (((Genome 0, Size, Coverage), (Genome 1, Size, Coverage), Intergenomic edit distance), (Readset 0, Readset 1), ((Assembly 0, Assembly 1), ((Assembly 0 with mixing), Assembly 1 with mixing)), ((evaluation 0, evaluation 1), (mixing evaluation 0, mixing evaluation 1)))
type AsmEval = (Ratio Int, Ratio Int)
spadesExperiment2 :: Rand Int -> (Genome -> Rand Genome) -> Rand Int -> Int -> Rand Int -> Rand Int -> (String -> Rand String) -> Rand Bool -> Ratio Int -> Rand (((Genome, Int, Ratio Int), (Genome, Int, Ratio Int), Int), (ReadSet, ReadSet), (([(String, Sequence)], [(String, Sequence)]), ([(String, Sequence)], [(String, Sequence)])), ((AsmEval, AsmEval), (AsmEval, AsmEval)))
spadesExperiment2 gLenD mutator rLenD sections readsPerSectionD sectionLenD sequenceError chmD mixRatio =
  do len <- gLenD --Length of the genome
     g0 <- dnaHmmSynthesize 0.0 256 len
     g1 <- mutator g0
     r0 <- sequenceGenomeReadsBiased g0 sections readsPerSectionD sectionLenD rLenD sequenceError chmD
     r1 <- sequenceGenomeReadsBiased g1 sections readsPerSectionD sectionLenD rLenD sequenceError chmD
     r0' <- liftM ((++) r0) (randomSublistRatio mixRatio r1)
     r1' <- liftM ((++) r0) (randomSublistRatio mixRatio r0)
     let a0 = runProgramUnsafe Spades r0
         a1 = runProgramUnsafe Spades r1
         --Selection should be a small fraction, so it can be ignoredif it doesn't support existing information.  Should shoot for coverage 1?
         a0' = runProgramUnsafe Spades r0'
         a1' = runProgramUnsafe Spades r1'
         --aToEval :: Genome -> [(String, String)] -> Int
         --aToEval g l = evaluateAssemblyContigsLD g (map snd l)
         --aToEval :: Genome -> [(String, String)] -> Ratio Int
         --aToEval g l = jacardKmerAssemblyCoverage 100 g (map snd l)
         aToEval :: Genome -> [(String, String)] -> (Ratio Int, Ratio Int)
         aToEval g l = (jacardSubkmerAssemblyCoverage 100 g (map snd l), n50StatisticLength (map snd l))
      in return (((g0, length g0, readDepth g0 r0), (g1, length g1, readDepth g1 r1), levenshteinDistance defaultEditCosts g0 g1), (r0, r1), ((a0, a1), (a0', a1')), ((aToEval g0 a0, aToEval g1 a1), (aToEval g0 a0', aToEval g1 a1')))

--Some parameterizations of the assembly experiment:
--Spades is pretty picky, and doesn't like small data sets. Genome sizes much smaller than this are not recommended.
spadesExperiment2Easy =
  let gLenD = UniformEnum (2000, 3000)
      smallBigRatio a b
       | a < b = a % b
       | otherwise = b % a
      mutator g = Condition (mutateGenomeIterated 5 g) (\ g' -> (smallBigRatio (length g) (length g')) > 1 % 2)
      rLenD = UniformEnum (50, 125)
      sections = 128
      rCountD = UniformEnum (2, 10)
      regionLenD = UniformEnum (200, 3000)
      sequenceError = mutateRead $ mutateP 0.05 -- 1 / 20 probability of uniform mutation
      chmD = Flip 0.05 -- 1 / 20 probability of chimerization.
      mixRatio = 1 % 10
   in spadesExperiment2 gLenD mutator rLenD sections rCountD regionLenD sequenceError chmD mixRatio

--In this experiment, the genomes being assembled are longer and more distant, the reads are shorter, there are slightly fewer reads (these factors all result in much lower coverage), and bias is more signficant.
spadesExperiment2Hard =
  let gLenD = UniformEnum (3000, 6000)
      smallBigRatio a b
       | a < b = a % b
       | otherwise = b % a
      mutator g = Condition (mutateGenomeIterated 8 g) (\ g' -> (smallBigRatio (length g) (length g')) > 1 % 2)
      rLenD = UniformEnum (50, 75)
      sections = 128
      rCountD = UniformEnum (1, 10)
      regionLenD = UniformEnum (100, 2000)
      sequenceError = mutateRead $ mutateP 0.1 -- 1 / 10 probability of uniform mutation
      chmD = Flip 0.1 -- 1 / 10 probability of chimerization.
      mixRatio = 1 % 10
   in spadesExperiment2 gLenD mutator rLenD sections rCountD regionLenD sequenceError chmD mixRatio

--We expect Spades to be capable of assembling the genomes without requiring the mixed in reads in the Easy experiment.  The addition of these reads may help assembly, but they may only serve to add confusion.
--We expect Spades to assemble the genomes very poorly in the Hard experiment, but we expect substantially improved performance with the addition of mixed reads.

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
  let (ak, bk) = mapT2 (kmerizeSequenceSet k) (a, b) --List of kmers
      (aks, bks) = mapT2 lexicographicSort (ak, bk) --Sorted lists of kmers.
      al = allListsSorted k nucleotides --All possible kmers (in sorted order).
      (aks', bks') = mapT2 (merge al) (aks, bks) --Sorted lists of kmers with pseudocount 1.
      (ar, br) = mapT2 Rosalyn.Random.fromSortedList (aks', bks')
      dist = symKlDiscrete ar br
   in sqrt $ Data.Maybe.fromJust dist --TODO is this actually a metric?
   --in if Data.Maybe.isNothing dist then -1 else Data.Maybe.fromJust dist

--Jensen-Shannon distance
jsReadsetDistance :: Int -> ReadSet -> ReadSet -> Double
jsReadsetDistance k a b =
  let ak = kmerizeSequenceSet k a
      bk = kmerizeSequenceSet k b
      ar = fromList ak
      br = fromList bk
   in sqrt $ jsDiscrete ar br

--Subramanian & Shwartz metric

--TODO S&S metric.

--This experiment is designed to evaluate readset distance metrics.
--It works by generating a readset for a genome and a closely related genome, sequencing reads for each, splitting the readset into a random partition, and evaluating the intra and inter species readset distances.  The AUROC is then taken between the intra and inter readset distances.  A good metric will have lower intraspecies readset distances than interspecies readset distances, and thus an AUROC near 1. 
--TODO parameterize to take original genomes and sequencing parameters.
--TODO add read correction: SPADES BayesHammer should work.
readsetEvaluationExperiment = 
  let gLenD = UniformEnum (1000, 2000)
      rLenD = UniformEnum (5, 50)
      sections = 32
      readsPerSectionD = UniformEnum (3, 5)
      sectionLenD = UniformEnum (200, 2000)
      mutator = mutateRead $ mutateP 0.1 -- 1 / 10 probability of uniform mutation
      chmD = Flip 0.1 -- 1 / 10 probability of chimerization.
   in do len <- gLenD --Length of the genome
         g0 <- dnaHmmSynthesize 0.0 256 len
         g1 <- mutateGenomeIterated 5 g0
         r0 <- sequenceGenomeReadsBiased g0 sections readsPerSectionD sectionLenD rLenD mutator chmD
         r1 <- sequenceGenomeReadsBiased g1 sections readsPerSectionD sectionLenD rLenD mutator chmD
         --evaluation <- mapM (const $ (evaluateReadsetDistanceMetric r0 r1 (\ a b -> realToFrac $ readsetDistance 8 a b))) [0..10]
         evaluation <- mapM (const $ (evaluateReadsetDistanceMetricSubset r0 r1 (\ a b -> realToFrac $ jsReadsetDistance 3 a b))) [0..50]
         let (intra, inter) = (concat $ map fst evaluation, concat $ map snd evaluation)
             --(intraF, interF) = (filter ((<=) 0) intra, filter ((<=) 0) inter)
          in return (evaluation, auroc intra inter)

--This experiment evaluates distance matrices in a phylogenetic context.

--Given: initial genome size distribution, phylogeny size distribution, number of genome mutations per descent, number of readsets to sample, and readset distance function.
--Constructs a random phylogeny from the output, draws readsets.  Then the following processing occurs:
--The AUROC for the readset distances is calculated, the classes being intragenomic and intergenomic distances.
--The distance matrix defined by the tree and the distance matrix produced by the distance metric are calculated.
--The euclidean distance between the normalized distance metrics is calculated.  This summary statistic determines how well the readset distance function matches the true distance function.
--Result: ((true DM, readset DM), DM distance, auroc, full phylogeny tree (including genomes and reads))
--TODO return read depth.
--TODO return rank difference of distance matrices (sum of offsets?  or number of swaps required?).
phylogenyDistanceExperiment :: Rand Int -> Rand Int -> Int -> (ReadSet -> ReadSet -> Double) -> Rand ((DistanceMatrix, DistanceMatrix), Double, Ratio Int, BinaryTree (Genome, [ReadSet]))
phylogenyDistanceExperiment gLenD tSizeD sampleCount d =
  do len <- gLenD --Length of the genome
     hmm <- (dnaHMM 0.0 256) --Random HMM to generate the genome
     --(h, g0) <- liftM (fromList . unzip) (hmmRand hmm 0 len) --The genome generated from the HMM
     (h, g0) <- liftM unzip (hmmRand hmm 0 len) --The genome generated from the HMM
     tSize <- tSizeD
     p <- randomPhylogeny g0 tSize 5 --TODO generalize this (5 mutations per speciation).
     rsLists <- mapM (\g -> replicateM sampleCount (sequenceGenomeReadsBiased' basicSequencerSpec g)) (inOrder p)
     let p' :: BinaryTree (Genome, [ReadSet])
         p' = zipTreeList p rsLists
         auc :: Ratio Int
         auc = uncurry auroc $ evaluateReadsetDistanceMetric rsLists d
         rsSingle :: [ReadSet]
         rsSingle = map head rsLists
         rsDM :: DistanceMatrix
         rsDM = createDistanceMatrix rsSingle d
         trueDM :: DistanceMatrix
         trueDM = constantWeightTreeDistanceMatrix 1 p
         dmDist = euclideanDistanceNormalizedDM trueDM rsDM
      in return ((trueDM, rsDM), dmDist, auc, p')

phylogenyDistanceExperimentJS :: Rand ((DistanceMatrix, DistanceMatrix), Double, Ratio Int, BinaryTree (Genome, [ReadSet]))
phylogenyDistanceExperimentJS = phylogenyDistanceExperiment (UniformEnum (1000, 2000)) (UniformEnum (4, 6)) 3 (jsReadsetDistance 7)
--phylogenyDistanceExperimentJS = phylogenyDistanceExperiment (UniformEnum (100, 200)) (UniformEnum (2, 3)) 2 (jsReadsetDistance 5)

--TODO make an experiment that plots performance against read depth for multiple readset distances.
