{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances, OverloadedLists, TypeFamilies, CPP, MultiParamTypeClasses #-}
module Rosalyn.Sequence where 

--This module contains code for interacting with DNA sequences.
--The primary goal is to get the lazy ByteStrings used by Bio.Core.SeqData to work as much like lists as is possible.

import GHC.Generics (Generic)
import GHC.Exts
import GHC.Word

import Data.Char
import Data.Hashable
import Data.String
import Data.Word
import Data.Int

--import Prelude hiding (map, length, replicate, reverse)
import Prelude hiding (length, head, last, null, tail, map, filter, concat, any, lookup, init, all, foldl, foldr, foldl1, foldr1, maximum, minimum, iterate, span, break, takeWhile, dropWhile, reverse, zip, zipWith, sequence, sequence_, mapM, mapM_, concatMap, and, or, sum, product, repeat, replicate, cycle, take, drop, splitAt, elem, notElem, unzip, lines, words, unlines, unwords)
import Data.ListLike

--import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Bio.Core.Sequence

import Rosalyn.Random

type Nucleotide = Char
type Nucleotide8 = Word8

nucToNuc8 :: Nucleotide -> Nucleotide8
nucToNuc8 = fromIntegral . ord

nuc8ToNuc :: Nucleotide8 -> Nucleotide
nuc8ToNuc = chr . fromIntegral

fNucToNuc8 :: (Functor f) => (Nucleotide -> f Nucleotide) -> (Nucleotide8 -> f Nucleotide8)
fNucToNuc8 f v = fmap (nucToNuc8) (f $ nuc8ToNuc v)

-- #define BIO_CORE_SEQUENCE
#ifdef BIO_CORE_SEQUENCE

--newtype Sequence = Data.ByteString deriving (BioSeq, IsList, IsString, FoldableLL, ListLike, ListLikeIO, StringLike)

type Sequence = SeqData

--It seems as though these StandaloneDeriving instances don't use GeneralizedNewtypeDeriving.
{-
deriving instance Hashable Sequence
--Instance the IsList class.  This allows the use of Sequence as an overloaded list.
deriving instance IsList Sequence
deriving instance IsString Sequence
--Instance the ListLike class (and related).  This allows the use of Sequence with generic list functions.
deriving instance FoldableLL Word8 Sequence
deriving instance ListLike Word8 Sequence
deriving instance ListLikeIO Sequence Word8
deriving instance StringLike Sequence
-}

--TODO there is a GHC bug, should be reported:
{-
  deriving instance ListLike Sequence Word8

  Seems to be incompatible with GHC.Word, StandaloneDeriving, and DeriveAnyClass.
  
  Probably an issue to do with deriving, aggravated by using so many language extension, and primitives.
-}

instance Hashable Sequence where
  hashWithSalt i (SeqData s) = hashWithSalt i s
  hash (SeqData s) = hash s

instance IsList Sequence where
  type (Item Sequence) = Char
  fromList l = SeqData $ Data.ByteString.Lazy.pack (map (fromIntegral . ord) l) --TODO make sure this isn't packing integers into 4 bytes.
  toList (SeqData s) = map (chr . fromIntegral) $ Data.ByteString.Lazy.unpack s
--TODO can we instance IsList multiple times for different types?
--This would provide substantially more transparancy.

instance FoldableLL Sequence Word8 where
  foldl f z (SeqData bs) = foldl f z bs
  foldr f z (SeqData bs) = foldr f z bs
instance ListLike Sequence Word8 where
  singleton = SeqData . singleton
  head (SeqData bs) = head bs
  tail (SeqData bs) = SeqData $ tail bs
  take c (SeqData bs) = SeqData (take c bs)
  drop c (SeqData bs) = SeqData (drop c bs)
  --map f (SeqData bs) = SeqData (map f bs)
instance ListLikeIO Sequence Word8
instance StringLike Sequence

--TODO this is a mess: nucleotide related operations should be abstracted better.
complement = complementW8

#else
type Sequence = [Nucleotide]

complement = complementChar
#endif

type Genome = Sequence
type SRead = Sequence
type Kmer = Sequence

type ReadSet = [Sequence]

--Basic nucleotide operations

complementChar :: Nucleotide -> Nucleotide
complementChar 'A' = 'T'
complementChar 'T' = 'A'
complementChar 'G' = 'C'
complementChar 'C' = 'G'

complementW8 :: Word8 -> Word8
complementW8 w = (fromIntegral . ord) (complementChar $ (chr . fromIntegral) w)

reverseComplement :: Sequence -> Sequence
reverseComplement = (map complement) . reverse

reverseComplementChar :: String -> String
reverseComplementChar = (map complementChar) . reverse

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


