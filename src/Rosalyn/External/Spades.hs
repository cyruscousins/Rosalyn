{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables, InstanceSigs #-}

module Rosalyn.External.Spades where

import GHC.Generics (Generic)

import System.IO.Unsafe

import Data.List
import Data.Char
import Data.Hashable

import Control.Monad

--import Language.Pads.Padsc

import Rosalyn.Executor
import Rosalyn.Random
import Rosalyn.Sequence
import Rosalyn.ListUtils

------------------------
--Draft Spades interface

--Singleton Spades type
data Spades = Spades
spades = Spades

instance Executable Spades [SRead] [(SRead, SRead)] where
  binaryName :: Spades -> String
  binaryName _ = "python"
  subName :: Spades -> String
  subName _ = "spades"
  arguments :: Spades -> [SRead] -> [SRead]
  arguments _ _ = ["origin/external_bin/SPAdes-3.6.2-Linux/bin/spades.py", "--only-assembler", "--careful", "--sc", "--s1", "reads.fa", "-o", "out/"]
  writeInput :: Spades -> [SRead] -> IO ()
  writeInput _ reads =
    let outFile = "reads.fa"
        outData = toFasta reads
     in writeFile outFile outData
  readOutput :: Spades -> [SRead] -> IO [(SRead, SRead)]
  readOutput _ _ = liftM fromFasta (readFile ("out/contigs.fasta"))

-------------------------
--PADS data definitions--

{-
ws :: RE
ws = REd "[ \t\n\r]*" " "

labelRE :: RE
labelRE = REd "[a-zA-Z0-9]+" " "

seqRE :: RE
seqRE = REd "[ATGC]+" " "

seqRE :: RE
seqRE = REd "[^\0]+" " "

[pads|
  type FASTA = [Line FLine]
  type FASTQ = [Line FQLine]
  
  data WSLine = ws
  data LabelLine = { '>', labelRE }
  data SequenceLine = { seqRE }
  data QualityLine = { qualiyRE }
  
  data FLine = { WSLine | LabelLine | SequenceLine }
  data FQLine = { WSLine | LabelLine | SequenceLine | QualityLine }
|]
-}

--------------------
--Spades assembler--

data CoverageCutoff = Auto | Off | Cutoff Float deriving (Eq, Generic, Hashable)
--data PhredOffset = P33 | P64
data SpadesConfig = SpadesConfig {
  sc :: Bool, ionTorrent :: Bool,
  onlyErrorCorrection :: Bool, onlyAssembly :: Bool,
  careful :: Bool, disableRR :: Bool, --Note: Careful turns on error correction, and is recommended.
  coverageCutoff :: CoverageCutoff, k :: [Int]
  } deriving (Eq, Generic, Hashable)

--TODO this isn't really specific to SPADES.
--TODO conceptually (funcionally), is there a difference beween the data known as "paired end" reads and "mate pair" reads?
data ReadsetType =
   Unpaired [Sequence]
 | UnpairedQ [[(Nucleotide, Phred)]]
 | Paired [(String, String)] -- (Rand Int) -- TODO Need some sort of Rand Eq.
 | PairedQ [([(Nucleotide, Phred)], [(Nucleotide, Phred)])] -- (Rand Int)
 deriving (Eq, Generic, Hashable)
 
--   PairedEnd [(String, String)] (Rand Int) --Paired end reads have a distribution over gap lengths.
-- | MatePair [(String, String)] (Rand Int) (Rand Bool) --Mate pair reads have a distribution over gap lengths.  I threw in a chimer frequency, though I'm not sure how this information is used, nor is it unapplicable to paired end reads.

data SpadesDataset = 
   PairedReadset [(String, String)] --TODO are some of them supposed to be RC?
 | UnpairedReadset [String] --TODO how are RC reads handled here?
 | TrustedContigs [String]
 | UntrustedContigs [String]
 deriving (Eq, Generic, Hashable)

data SpadesInput = SpadesInput SpadesConfig [SpadesDataset] deriving (Eq, Generic, Hashable)
{-
SPAdes genome assembler v.3.6.2

Usage: ./SPAdes-3.6.2-Linux/bin/spades.py [options] -o <output_dir>

Basic options:
-o	<output_dir>	directory to store all the resulting files (required)
--sc			this flag is required for MDA (single-cell) data
--iontorrent		this flag is required for IonTorrent data
--test			runs SPAdes on toy dataset
-h/--help		prints this usage message

Input data:
--12	<filename>	file with interlaced forward and reverse paired-end reads
-1	<filename>	file with forward paired-end reads
-2	<filename>	file with reverse paired-end reads
-s	<filename>	file with unpaired reads
--pe<#>-12	<filename>	file with interlaced reads for paired-end library number <#> (<#> = 1,2,3,4,5)
--pe<#>-1	<filename>	file with forward reads for paired-end library number <#> (<#> = 1,2,3,4,5)
--pe<#>-2	<filename>	file with reverse reads for paired-end library number <#> (<#> = 1,2,3,4,5)
--pe<#>-s	<filename>	file with unpaired reads for paired-end library number <#> (<#> = 1,2,3,4,5)
--pe<#>-<or>	orientation of reads for paired-end library number <#> (<#> = 1,2,3,4,5; <or> = fr, rf, ff)
--s<#>		<filename>	file with unpaired reads for single reads library number <#> (<#> = 1,2,3,4,5)
--mp<#>-12	<filename>	file with interlaced reads for mate-pair library number <#> (<#> = 1,2,3,4,5)
--mp<#>-1	<filename>	file with forward reads for mate-pair library number <#> (<#> = 1,2,3,4,5)
--mp<#>-2	<filename>	file with reverse reads for mate-pair library number <#> (<#> = 1,2,3,4,5)
--mp<#>-s	<filename>	file with unpaired reads for mate-pair library number <#> (<#> = 1,2,3,4,5)
--mp<#>-<or>	orientation of reads for mate-pair library number <#> (<#> = 1,2,3,4,5; <or> = fr, rf, ff)
--hqmp<#>-12	<filename>	file with interlaced reads for high-quality mate-pair library number <#> (<#> = 1,2,3,4,5)
--hqmp<#>-1	<filename>	file with forward reads for high-quality mate-pair library number <#> (<#> = 1,2,3,4,5)
--hqmp<#>-2	<filename>	file with reverse reads for high-quality mate-pair library number <#> (<#> = 1,2,3,4,5)
--hqmp<#>-s	<filename>	file with unpaired reads for high-quality mate-pair library number <#> (<#> = 1,2,3,4,5)
--hqmp<#>-<or>	orientation of reads for high-quality mate-pair library number <#> (<#> = 1,2,3,4,5; <or> = fr, rf, ff)
--nxmate<#>-1	<filename>	file with forward reads for Lucigen NxMate library number <#> (<#> = 1,2,3,4,5)
--nxmate<#>-2	<filename>	file with reverse reads for Lucigen NxMate library number <#> (<#> = 1,2,3,4,5)
--sanger	<filename>	file with Sanger reads
--pacbio	<filename>	file with PacBio reads
--nanopore	<filename>	file with Nanopore reads
--trusted-contigs	<filename>	file with trusted contigs
--untrusted-contigs	<filename>	file with untrusted contigs

Pipeline options:
--only-error-correction	runs only read error correction (without assembling)
--only-assembler	runs only assembling (without read error correction)
--careful		tries to reduce number of mismatches and short indels
--continue		continue run from the last available check-point
--restart-from	<cp>	restart run with updated options and from the specified check-point ('ec', 'as', 'k<int>', 'mc')
--disable-gzip-output	forces error correction not to compress the corrected reads
--disable-rr		disables repeat resolution stage of assembling

Advanced options:
--dataset	<filename>	file with dataset description in YAML format
-t/--threads	<int>		number of threads
				[default: 16]
-m/--memory	<int>		RAM limit for SPAdes in Gb (terminates if exceeded)
				[default: 250]
--tmp-dir	<dirname>	directory for temporary files
				[default: <output_dir>/tmp]
-k		<int,int,...>	comma-separated list of k-mer sizes (must be odd and
				less than 128) [default: 'auto']
--cov-cutoff	<float>		coverage cutoff value (a positive float number, or 'auto', or 'off') [default: 'off']
--phred-offset	<33 or 64>	PHRED quality offset in the input reads (33 or 64)
				[default: auto-detect]
-}

--TODO use pads for this stuff!
--TODO biohaskell compatibility.
toFasta :: [String] -> String
toFasta l = concat (map (\ (i, r) -> ">seq" ++ (show i) ++ "\n" ++ r ++ "\n") (zip [0..] l))

fromFasta :: String -> [(String, String)]
fromFasta s =
  let l = lines s
      fHelper curLine [] = [curLine] --Out of lines
      fHelper (label, seq) (curLine:rest)
       | (head curLine) == '>' = (label, seq):(fHelper (curLine, []) rest)
       | otherwise = fHelper (label, seq ++ (curLine)) rest
   in (drop 1) (fHelper ("", "") l)
--fromFasta = disjointAdjacentPairs . lines



coverageToArgs :: CoverageCutoff -> [String]
coverageToArgs Auto = ["--cov-cutoff", "auto"]
coverageToArgs Off =  ["--cov-cutoff", "off"]
coverageToArgs (Cutoff c) = ["--cov-cutoff", (show c)]

--phredToArgs ::

--TODO need to figure out if auto is different from nothing (does it override --sc?)
kToArgs :: [Int] -> [String]
kToArgs [] = []
kToArgs l = ["-k", (intercalate "," (map show l))]

--phred-offset	<33 or 64>
configToArgs :: SpadesConfig -> [String]
configToArgs SpadesConfig {sc = sc, ionTorrent = ionTorrent, onlyErrorCorrection = onlyErrorCorrection, onlyAssembly = onlyAssembly, careful = careful, disableRR = disableRR, coverageCutoff = coverageCutoff, k = k } =
  let scArg = if sc then ["--sc"] else []
      ionArg = if ionTorrent then ["--iontorrent"] else []
      oecArg = if onlyErrorCorrection then ["--only-error-correction"] else []
      oasmArg = if onlyAssembly then ["--only-assembler"] else []
      carefulArg = if careful then ["--careful"] else []
      rrArg = if disableRR then ["--disable-rr"] else []
      covArg = coverageToArgs coverageCutoff
      kArg = kToArgs k
   in concat [scArg, ionArg, oecArg, oasmArg, carefulArg, rrArg, covArg, kArg]

spadesArgs' :: SpadesInput -> String -> [String]
spadesArgs' (SpadesInput config _) dir = ["external_bin/SPAdes-3.6.2-Linux/bin/spades.py", "--dataset", dir ++ "dataset.yaml", "-o", dir ++ "out/"] ++ (configToArgs config)

data Orientation = FR | RF
data SpadesReadType = PairedEnd | MatePair | Single | PacBio 

orientationString :: Orientation -> String
orientationString FR = "fr"
orientationString RF = "rf"

readTypeString :: SpadesReadType -> String
readTypeString PairedEnd = "paired-end"
readTypeString MatePair = "mate-pairs"
readTypeString Single = "single"
readTypeString PacBio = "pac-bio"

readTypeReadModeString :: SpadesReadType -> String
readTypeReadModeString PairedEnd = "interlaced"
readTypeReadModeString MatePair = "interlaced"
readTypeReadModeString Single = "single"
readTypeReadModeString PacBio = "single"

readFileName :: SpadesReadType -> Int -> String
readFileName _ i = "reads" ++ (show i) ++ ".fa"

yamlEntry :: String -> Orientation -> SpadesReadType -> Int -> String
yamlEntry dir o rt idx = "{ " ++ "orientation: \"" ++ (orientationString o) ++ "\", type: \"" ++ (readTypeString rt) ++ "\", " ++ (readTypeReadModeString rt) ++ ", reads: [" ++ (readFileName rt idx) ++ "]"

{-
spadesOut' :: SpadesInput -> String -> IO ()
spadesOut' (SpadesInput cfg readsets) dir =
  let processReadset :: Int -> SpadesDataset -> IO String
      processReadset i (PairedReadset r) = undefined
      processReadset i (UnpairedReadset r) =
        let fileName = dir ++ "reads" ++ (show i) ++ ".fa"
            yamlEntry = "- single reads: [" ++ fileName ++ "]]\n  type: single\n"
         in fmap (const yamlEntry) $ writeFile fileName (toFasta r)
      processReadset i (TrustedContigs r) = undefined
      processReadset i (UntrustedContigs r) = undefined
      --TODO need to be able to process quality scores too.
      indexedReadsets = zip [0..] readsets
      yamlFile = dir ++ "datasets.yaml"
   in do yamlEntries <- mapM (uncurry processReadset) (zip [0..] readsets)
         writeFile yamlFile (concat yamlEntries)

spadesIn' :: SpadesInput -> String -> IO [(String, String)]
spadesIn' _ = spadesIn [] --TODO need to handle onlyAssembly case.

spades' :: Rosalyn.Executor.Program SpadesInput [(String, String)]
spades' = Rosalyn.Executor.Program "python" "spades" spadesArgs' spadesOut' spadesIn' (const [])

spadesUnsafe' :: SpadesInput -> [(String, String)]
spadesUnsafe' input = unsafePerformIO $ runProgram spades' input
-}

--Spades installation (linux):
{-
wget http://spades.bioinf.spbau.ru/release3.5.0/SPAdes-3.5.0-Linux.tar.gz
tar -xzf SPAdes-3.5.0-Linux.tar.gz
cd SPAdes-3.5.0-Linux/bin/
-}


