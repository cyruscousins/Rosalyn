{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeSynonymInstances, OverloadedLists, TypeFamilies, CPP #-}
module Rosalyn.Sequence where 

--This module contains code for interacting with DNA sequences.
--The primary goal is to get the lazy ByteStrings used by Bio.Core.SeqData to work as much like lists as is possible.

import GHC.Generics (Generic)
import GHC.Exts

import Data.Char
import Data.Hashable
import Data.String

--import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Bio.Core.Sequence

import Rosalyn.Random

type Nucleotide = Char

#ifdef BIO_CORE_SEQUENCE
--TODO preprocessor control: given an option to use a [Char]
type Sequence = SeqData

instance Hashable Sequence where --TODO can this be expressed more succinctly?
  hashWithSalt i (SeqData s) = hashWithSalt i (Data.ByteString.Lazy.unpack s)
  hash (SeqData s) = hash (Data.ByteString.Lazy.unpack s)

--Instance the IsList class.  This allows the use of Sequence as an overloaded list.  Buckle up; it's going to be a rough ride.
--The strategy is very unclean, as the definition only holds for lists of bytes.
--TODO
instance IsList Sequence where
  type (Item Sequence) = Char
  fromList l = undefined SeqData $ Data.ByteString.Lazy.pack l
  toList (SeqData s) = undefined--Data.ByteString.Lazy.unpack s

--Used mostly for mapping: ideally this construct should not be used as it is inherently inefficient.
emptySequence :: Int -> Sequence
emptySequence i = GHC.Exts.fromList (replicate i '\0')
#else
type Sequence = [Nucleotide]
#endif

type Genome = Sequence
type SRead = Sequence

type ReadSet = [Sequence]
type List a = [a]

--Basic nucleotide operations

complement :: Nucleotide -> Nucleotide
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'

--TODO want a generic version that works for bytestrings.
reverseComplement :: [Char] -> [Char]
reverseComplement = (map complement) . reverse

--nucleotides :: --(IsList l) => l
nucleotides = ['A', 'T', 'C', 'G']

randomNucleotide :: Rand Nucleotide
randomNucleotide = Uniform nucleotides

-----------
--Quality--

clamp :: (Ord a) => a -> a -> a -> a
clamp min val max
  | (min > val) = min
  | (max < val) = max
  | otherwise = val

--An integral Phred score, used in FASTQ and other data interchange.
--TODO need to refactor: Solexa/Illumina scores aren't actually Phred scores.
--Phred: Q = -10 log10 E
--Solexa/Illumina: Q = -10 log10 (E / (1-E))
data PhredFormat = Sanger | SolexaIllumina deriving (Eq, Generic, Hashable)

phredChr0 :: PhredFormat -> Char
phredChr0 Sanger = chr 33
phredChr0 SolexaIllumina = chr (59 + 5)

phredMinQ :: PhredFormat -> Int
phredMinQ Sanger = 0
phredMinQ SolexaIllumina = -5

phredMaxQ :: PhredFormat -> Int
phredMaxQ Sanger = 93
phredMaxQ SolexaIllumina = 62

--Phred quality scores.
data Phred = Phred PhredFormat Int deriving (Eq, Generic, Hashable)
instance Show Phred where
  show (Phred fmt val) = [chr ((ord $ phredChr0 fmt) + (clamp (phredMinQ fmt) val (phredMaxQ fmt)))]

--Continuous Phred score.  Used to convert between probabilities and integral Phred scores.  This representation is nearly lossless as compared to a floating point value, as it is essentially a scaled log probability with an odd base.
data PhredF = PhredF PhredFormat Prob deriving (Eq, Generic, Hashable)
instance Show PhredF where
  show = show . phredFToPhred

--TODO: These are wrong for Solexa/Illumina reads, especially for large error probabilities.

--TODO rounding to the nearest probability is not the same as rounding to the nearest log probability.  I believe we need to round down after adding log_10 (50) - log_10 (10) ~ .69897.
phredFToPhred :: PhredF -> Phred
phredFToPhred (PhredF fmt v)
  | v < 0 = undefined
  | v == 1.0 / 0.0 = Phred fmt 1000 --TODO this is totally arbitrary (but represents a very low probability).  This is done because of a strange floor implementation taking infinity onto 0.
  | otherwise = Phred fmt (floor (v + 0.69897))

probToPhredF :: PhredFormat -> Prob -> PhredF
probToPhredF fmt p
  | (p < 0) || (p > 1) = undefined
  | otherwise = PhredF fmt (-10 * (logBase 10 p))

probToPhred :: PhredFormat -> Prob -> Phred
probToPhred fmt = phredFToPhred . (probToPhredF fmt)

phredToPhredF :: Phred -> PhredF
phredToPhredF (Phred fmt v) = PhredF fmt (fromIntegral v)

phredFToProb :: PhredF -> Prob
phredFToProb (PhredF fmt v) = 10 ** (-v / 10)

phredToProb :: Phred -> Prob
phredToProb = phredFToProb . phredToPhredF


