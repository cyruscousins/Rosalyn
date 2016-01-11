module Main where

import Rosalyn.Random
import Rosalyn.Sequencing
import Rosalyn.Phylogeny

import Experiment.Experiment

import Control.Monad

--Execute some expensive experiments.  
--This program is designed to show off a few features and give Rosalyn a workout to generate profiling data.
main :: IO ()
main =
  let (g1:g2:g3:g4:g5:g6:_) = splitInfinity gen0
      (hmm, _) = sample (dnaHMM 0.0 512) gen0 --TODO Can we use a Monad Transformer to use the Random monad from within the IO Monad?
      (genome, _) = sample (liftM unzip (hmmRand hmm 0 4096)) g1
      (spadesAsm, _) = sample spadesExperiment2Easy g2
      (phylogeny, _) = sample (randomExtantPhylogeny (Return (snd genome)) (UniformEnum (4, 16))) g3
      (rspe, _) = sample readsetEvaluationExperiment g4
      (rdme, _) = sample (phylogenyDistanceExperimentJS) g5
   in do putStrLn "Synthetic Genome:"
         putStrLn $ show genome
         putStrLn "Spades Assembly Experiment:"
         putStrLn $ show spadesAsm
         putStrLn "Random Phylogeny:"
         putStrLn $ show $ phylogeny
         putStrLn "Readset Distance Pairwise Evaluation:"
         putStrLn $ show $ rspe
         putStrLn "Readset Distance Matrix Evaluation:"
         putStrLn $ show $ rdme

