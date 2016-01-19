{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DefaultSignatures, ExistentialQuantification, DeriveGeneric, DeriveAnyClass #-}
module Rosalyn.GnuPlot where

import GHC.Generics
import Data.List
import Data.Hashable

import Rosalyn.Executor


--This module allows the output of graphs with GNUplot.
--At present, image filepaths are produced, rather than the images themselves, as Rosalyn lacks an image representation.

data GPData = GPData [(Double, Double)] String deriving (Show, Generic, Hashable)
data GPScript = GPScript String String deriving (Show, Generic, Hashable)

instance Executable GPScript [GPData] String where
  binaryName _ = "gnuplot"
  inputToString (GPScript title style) dat =
    let prescript = intercalate "\n" [
            "set terminal png",
            "set output 'plot.png'",
            "set title \"" ++ title ++ "\""
          ]
        commandStr = "plot " ++ (intercalate ", " commands)
        commands = map (\ (GPData _ title) -> concat ["'", title, ".dat' using 1:2 title '", title, "' with ", style]) dat
     in intercalate "\n" [prescript, commandStr]
  writeInput p dat =
    do writeFile "stdin" (inputToString p dat)
       mapM_ (\ (GPData dat' title) -> writeFile (title ++ ".dat") (intercalate "\n" $ map (\ (a, b) -> (show a) ++ " " ++ (show b)) dat')) dat
  readOutput p a = return $ (fullDirectory p a) ++ "plot.png"

