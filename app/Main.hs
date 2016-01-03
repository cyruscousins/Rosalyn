module Main where

import Rosalyn.Random

import Experiment.Experiment

main :: IO ()
main =
  do putStrLn $ show $ (sample readsetEvaluationExperiment gen0)

