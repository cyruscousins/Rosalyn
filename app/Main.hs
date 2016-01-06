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
      (phylogeny, g3) = sample (randomGenomePhylogeny (Return (snd genome)) (UniformEnum (4, 16))) g2
   in do putStrLn "Synthetic Genome:"
         putStrLn $ show genome
         --putStrLn "Spades Assembly:"
         putStrLn "Readset Distance Evaluation:"
         putStrLn $ show $ (sample readsetEvaluationExperiment gen0)
         putStrLn "Random Phylogeny:"
         putStrLn $ show $ phylogeny
