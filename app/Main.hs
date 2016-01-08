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
  let (hmm, g1) = sample (dnaHMM 0.0 512) gen0 --TODO Can we use a Monad Transformer to use the Random monad from within the IO Monad?
      (genome, g2) = sample (liftM unzip (hmmRand hmm 0 4096)) g1
      (phylogeny, g3) = sample (randomExtantPhylogeny (Return (snd genome)) (UniformEnum (4, 16))) g2
      (rspe, g4) = sample readsetEvaluationExperiment g3
      (rdme, g5) = sample (phylogenyDistanceExperimentJS) g4
   in do putStrLn "Synthetic Genome:"
         --putStrLn $ show genome
         putStrLn "Spades Assembly:"
         putStrLn "Random Phylogeny:"
         putStrLn $ show $ phylogeny
         putStrLn "Readset Distance Pairwise Evaluation:"
         putStrLn $ show $ rspe
         putStrLn "Readset Distance Matrix Evaluation:"
         putStrLn $ show $ rdme
