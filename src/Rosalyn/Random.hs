{-# LANGUAGE GADTs, ExistentialQuantification, InstanceSigs, ScopedTypeVariables #-}

module Rosalyn.Random where

import Rosalyn.ListUtils

import Data.List
import Data.Maybe
import Data.Array.Unboxed
import qualified Data.Map

import System.Random

import Control.Monad

import GHC.Exts -- sortWith

--TODO This should be a typeclass.
type Prob = Double

--class Sample 

{-
class Random a where
  trySample :: Random a -> StdGen -> (Maybe a, StdGen)
  sample :: Random a -> StdGen -> (a, StdGen)
  --TODO consider versions of these functions that discard the StdGen, particularly when it may cause divergence
  weightedSampleStream :: Random a -> StdGen -> ([(a, Prob)], StdGen)
  nonzeroWeightedSampleStream :: Random a -> StdGen -> ([(a, Prob)], StdGen)
  nonzeroWeightedSampleStream r g = filter (\ (a, p) -> (p /= 0))
  --TODO support, maximum likelihood, ...
-}

--NOTE: For the time being, this is ordinary probability monad.  In the future, the following will hold:
{-
Despite the name, this isn't a pure probability monad.  Several paradox resolutions have been applied in a manner that may scare small children, brave adults, probability practitioners, and theoretical mathematicians.

Abnormalities discussed here:

Impossible Observation Paradox: When an observation is made that results in an empty possibility set, the observation may be entirely ignored.

Contradictory Observation Paradox: When contradictory observations are made using an ObserveMulti, values satisfying a maximal number of observations are returned.  Exactly how this is done is configurable.
-}

--Rand GADT
data Rand a where
  --
  --Probability Monad with Conditioning
  --
  Return :: a -> Rand a
  Condition :: Rand a -> (a -> Bool) -> Rand a
  Bind :: Rand b -> (b -> Rand a) -> Rand a
  --
  --Generic Distribution Constructors
  --
  Uniform :: [a] -> Rand a
  --UniformOrd :: [a] -> Rand a
  WeightedCMF :: [(a, Prob)] -> Rand a --TODO these should be stored in a map or an array.
  WeightedCMFArray :: Array Int a -> UArray Int Prob -> Rand a
  --An efficient data structure for prob queries. --TODO could replace with array + binary search? --TODO want to specialize when UArray is possible.
  WeightedOrd :: (Ord a) => Data.Map.Map a Prob -> Rand a
  --Perhaps this primitive is surprising.  However, alternatives require a great amount of data.
  UniformList :: [a] -> Int -> Rand [a]
  --
  --
  ----Specialized Generic Distribution Constructors
  ----These are more efficient than the generic distribution constructors.
  --
  ----Uniform distribution over all possible values of a finite type.
  UniformBounded :: (Enum a, Eq a, Bounded a) => Rand a
  ----Uniform Distribution over a convex discrete enumerable subset.  Unfortunately, "discrete" isn't captured in the type.  Storage cost is low, and sampling can be done with integers.  --TODO clarify usage of term convex.
  UniformEnum :: (Enum a, Eq a) => (a, a) -> Rand a
  ----Uniform Distribution over a convex continuous enumerable subset.  Low storage cost.--TODO probably need more constraints to get something that can be sampled from.
  --UniformOrd  :: (Ord a) => (a, a) -> Rand a
  --
  ----Boolean Distributions.  Every distribution over booleans can be represented with a single application of the following combinator:
  --
  Flip        :: Prob -> Rand Bool
  --
  ----Trivial Integer Distributions (Not particularly interesting, can all be implemented with Flip and map).
  --
  --Bernoulli   :: (Integral i) => Prob -> Rand i
  --Rademacher  :: (Integral i) => Rand i
  --
  ----Integer Distributions
  --
  --Binomial    :: (Integral i) => Prob -> i -> Rand i
  --Poisson     :: (Integral i) => Prob -> Rand i
  --Hypergeometric :: (Integral i) => Prob -> i -> i -> Rand i
  --Poisson_Binomial :: (Integral i) => [Prob] -> Rand i
  --
  ----Real Distributions
  --
  UniformReal :: (RealFrac r, Random r) => (r, r) -> Rand r
  Gaussian :: (RealFrac r) => r -> r -> Rand r
  Exponential :: (RealFrac r) => r -> Rand r
  --Beta     :: (RealFrac r) => r -> r -> Rand r
  --Kumaraswamy :: (RealFrac r) => r -> r -> Rand r
  --Gamma    :: (Integral i, RealFrac r) => i -> r -> Rand r
  --Bates    :: (Integral i, RealFrac r) => i -> Rand r
  --Chi      :: (Integral i, RealFrac r) => i -> Rand r
  --ChiSqr   :: (Integral i, RealFrac r) => i -> Rand r
  --Cauchy   :: (RealFrac r) -> Rand r
  --
  ----Complicated Generic Distributions
  --
  --Dirichlet
  ----Multinomial Distribution.
  --Multinomial :: (Ord a, Integral i) => Rand a -> i -> Rand [a, i]
  --
  
  --Refer to https://en.wikipedia.org/wiki/List_of_probability_distributions#With_finite_support for more useful distributions.

--data Primitive a where

--These are unusual or sophisticated distributions.
--They are mostly intended for optimizations and alternative data representations. 
--They may always be converted into simpler distributions.
--data ExoticRand a where
  ----These alternative representations are potentially much faster than the above.  Intermediate forms used in the optimization process are also represented here.
  --
  --ObserveMulti :: Rand a -> [(a -> Bool)] -> Rand a
  --ObserveInterval :: (Ord a) => Rand a -> (a, a) -> Rand a
  --ObserveIntervalMulti :: (Ord a) => Rand a -> [(a, a)] -> Rand a
  --ConvolutionN :: (Num a) => [Rand a] -> Rand a
  --UniformMixture :: [Rand a] -> Rand a
  --Mixture :: Rand (Rand a) -> Rand a
  --BinaryCombination :: Rand a -> Rand b -> (a -> b -> c) -> Rand c
  --IndependentJointDistribution :: Rand a -> Rand b -> Rand (a, b)
  --QuotientDistribution :: Rand (a, b) -> Rand a -> Rand b --I'm not sure about this one: it's not necessarily a distribution.
  --MarginalizeDistribution :: Rand (a, b) -> Rand a

--TODO factor primitives and optimizations into their own modules.

--TODO Want to do some specializations based on typeclasses.
--{-# RULES "WeightedCMF/Ord" WeightedCMF = WeightedOrd #-}

instance Functor Rand where
  fmap f r = Bind r (Return . f)

instance Applicative Rand where
  pure = Return
  (<*>) f r = Bind f (\ f -> Bind r (Return . f))

instance Monad Rand where
  (>>=) = Bind
  return = Return


--Creator functions:
fromSortedList :: (Ord a) => [a] -> Rand a
fromSortedList a =
  let grouped = group a
      counted = map (\l -> (head l, length l)) grouped
      total = sum (map snd counted)
      weighted = map (\(a, b) -> (a, (fromIntegral b) / (fromIntegral total))) counted
   in WeightedOrd $ Data.Map.fromList weighted

fromList :: (Ord a) => [a] -> Rand a
fromList = fromSortedList . sort


--Given a list of cumulative masses, produce an array representation.
toWeightedCMF :: [(a, Prob)] -> Rand a
toWeightedCMF l = 
  let len = length l
      aLookup = array (0, (pred len)) (indexList $ map fst l)
      pLookup = array (0, (pred len)) (indexList $ map snd l)
   in WeightedCMFArray aLookup pLookup

--TODO remove: These are now primitives.
coinFlip :: Prob -> Rand Bool
coinFlip p
 | (>) 0 p = undefined
 | (<) 1 p = undefined
 | otherwise = WeightedCMF [(False, p), (True, 1)]

bernoulli :: Prob -> Rand Int
bernoulli f = 
  let c False = 0
      c True = 1
   in fmap c (coinFlip f)

geometric :: Prob -> Rand Int
geometric f =
  do b <- coinFlip f
     if b then fmap ((+) 1) (geometric f) else Return 0

--Attempt to produce a single sample (fails if a condition fails).
trySample :: (Rand a) -> StdGen -> (Maybe a, StdGen)
trySample (Return a) g = (Just a, g)
trySample (Uniform l) g =
  let (i, g2) = randomR (0, (pred . length) l) g
   in (Just $ (!!) l i, g2)
trySample (WeightedCMF l) g0 =
  let (v, g1) = randomR (0, 1) g0
      (a, f) = fromJust (find (\ (a, cm) -> (<=) v cm) l)
   in (Just a, g1)
trySample (Condition r p) g0 =
  let (v, g1) = trySample r g0
   in if isNothing v 
      then (Nothing, g1)
      else if (p (fromJust v))
           then (v, g1)
           else (Nothing, g1)

--TODO it would be good to shuffle the list here when it is finite.  Maybe we should shuffle for finite lists?  Alternatively, we could randomly draw from the lists.
trySample (Bind r f) g0 =
  let (v0, g1) = trySample r g0
   in if isNothing v0
      then (Nothing, g1)
      else trySample (f (fromJust v0)) g1 --TODO this causes issues with random infinite lists.
trySample (UniformEnum (a, b)) g0 =
  let (v0, g1) = randomR ((fromEnum a), (fromEnum b)) g0
   in ((Just . toEnum) v0, g1)
trySample (UniformBounded) g0 = trySample (UniformEnum (minBound, maxBound)) g0
trySample (Flip p) g0 =
  let (v0, g1) = randomR (0, 1) g0
   in (Just ((<) v0 p), g1)
trySample (UniformReal (b0, b1)) g0 =
  let (u, g1) = randomR (b0, b1) g0
   in (Just u, g1)
trySample (Exponential lambda) g0 =
  --Inverse CFD sampling
  -- y = e^-lx
  -- log y = -lx
  -- x = - (log y) / l 
  let (u, g1) = sample (UniformReal (0, 1 :: Prob)) g0
      v = realToFrac $ -(log u) / (realToFrac lambda)
   in (Just v, g1)
--TODO Implement a more efficient Gaussian sampling algorithm.
trySample (Gaussian mu sigma) g0 =
  let (hn, g1) = sample (rejection 1 (prob (Gaussian 0 1)) (Exponential 1)) g0
      (flip, g2) = sample (Uniform [-1, 1]) g1 --TODO Rademacher.
   in (Just (realToFrac $ mu + (realToFrac sigma) * flip * hn), g2)

--Write your very own rejection sampler!
--TODO prob should not require Eq, which would remove this requirement too.
--TODO we should probably check to make sure one function dominates the other.
rejection :: (Eq a) => Prob -> (a -> Prob) -> (Rand a) -> Rand a
rejection c f0 f1 =
  do v <- f1
     Bind (Condition (UniformReal (0, (c * (prob f1 v)))) ((>) (f0 v))) (const (Return v))

importance :: (Eq a) => (a -> Prob) -> (Rand a) -> Rand (a, Prob)
importance f0 f1 =
  do v <- f1
     Return (v, (f0 v) / (prob f1 v))

sample :: (Rand a) -> StdGen -> (a, StdGen)
sample r g0 =
  let (v, g1) = trySample r g0
   in if isNothing v
      then sample r g1
      else (fromJust v, g1)

sampleStream :: (Rand a) -> StdGen -> [(a, StdGen)]
sampleStream r g0 =
  let generators = splitInfinity g0
   in map (\g -> sample r g) generators
   --TODO highly inefficient.

--Test Distributions:

dist0 :: Rand Int
dist0 = Bind (Uniform [UniformEnum (1, 4), UniformEnum (1, 6), UniformEnum (1, 8), UniformEnum (1, 12), UniformEnum (1, 20)]) id

gen0 :: StdGen
gen0 = System.Random.mkStdGen 161012311992

--List helper function.
--Alternates between lists.
alternate :: [[a]] -> [a]
alternate [] = []
alternate l =
  let alternate' l0 [] = alternate l0 --We've reached the end of the outer list.  Go back to the beginning (next column).
      alternate' l0 ([]:l1) = alternate' l0 l1 --The current list is empty.  Discard it and move on.
      alternate' l0 ((a:li):l1) = (:) a (alternate' (l0 ++ [li]) l1) --The current list is nonempty.  Take the item, and move on.
   in alternate' [] l

{-

l0  0  (n+1)  (2*n+1)  (3*n-3+1)  [empty]

l1  1  (n+2)  [empty]  [empty]

l2  2  (n+3)  (2*n+2)  [empty]

l3  3  (n+4)  [empty]  [empty]

.
.
.

ln  n  (2*n)  [3*n-3]  [empty]

-}

--Alternate causes early-list bias for infinite sets.
--This function does not.
alternateTriangular :: [[a]] -> [a]
alternateTriangular l =
  let alternate' l0 l1 oc 0 = alternate' [] (l0 ++ l1) (succ oc) (succ oc) --The count has been reached.  Return to the first list.
      alternate' l0 [] oc c = alternate l0 --Use the orininal alterate function, the count has reached the end of the outer list.
      alternate' l0 ([]:l1) oc c = alternate' l0 l1 oc c --The current list has been emptied.  Remove it and don't decrement the count. 
      alternate' l0 ((a:li):l1) oc c = (:) a (alternate' (l0 ++ [li]) l1 oc (pred c)) --Normal case: take
   in alternate' [] l 1 1

{-
0  1  3  6  10 15 ...
2  4  7  11 16
5  8  12 17
.
.
.
n
-}

splitInfinity :: StdGen -> [StdGen]
splitInfinity g0 =
  let (g1, g2) = split g0
   in (:) g1 (splitInfinity g2)

--Produce a stream of weighted samples from a distribution.
--When possible, actual sampling is avoided.
--Total sample mass is <= 1.
--Produces the true distribution for bounded finite distributions, if the fallback is not used.
--If fallback is used, produces the true distribution as infinite runs are averaged together.
--Cases like ((repeat 0) ++ [1]) are however not handled.  Mathematically, I don't really know how to describe these.
sampleWeighted :: (Rand a) -> StdGen -> ([(a, Prob)], StdGen)
sampleWeighted (Return a) g = ([(a, 1)], g)
sampleWeighted (Condition d p) g0 =
  let (s, g1) = sampleWeighted d g0
   in ((filter (p . fst) s), g1)
--  let attempts = foldl (\ (v, gi) i -> (:) (fromMaybe (fmap ((*) ((/) (fromIntegral 1) (fromIntegral (2^i))))) [1..]
--sampleWeighted (Condition d p) = --TODO.  Interesting idea: Negative sampling.  First pick 1.  Then pick (-1/2, 1/2).  Then pick (-1/3, 1/3).  Continue forever.  Note, we have to pick the negatives from things for which we've already picked positives (preferably the highest one).
sampleWeighted (Bind d f) g0 = --TODO use alternate.
  let (g1:g2:gi) = splitInfinity g0
      multiplyStream :: Prob -> [(a, Prob)] -> [(a, Prob)]
      multiplyStream p s = map (\(x, pi) -> (x, (*) p pi)) s
      --s0 :: ([(b, Prob)], StdGen)
      (l, _) = sampleWeighted d g1
      --sampleStreams :: [[(a, Prob)]]
      sampleStreams = map (\ ((a, p), g) -> multiplyStream p (fst $ sampleWeighted (f a) g)) (zip l gi) --TODO ensure no probability 0 events get through in the optimizer? They will slow down the works.
   in ((alternateTriangular sampleStreams), g2)
sampleWeighted (Uniform l) g0 =
  let len = length l
      p = (/) (fromIntegral 1) (fromIntegral len)
      (ls, g1) = sample (shuffleList l) g0
      samples = map (\x -> (x, p)) ls
   in (samples, g1)
sampleWeighted (WeightedCMF l) g =
  let vm = fromCumulativeValues l
   in (vm, g)
sampleWeighted (UniformBounded) g = sampleWeighted (UniformEnum (minBound, maxBound)) g
sampleWeighted (UniformEnum (b0, b1)) g = sampleWeighted (Uniform [b0..b1]) g
sampleWeighted (Flip p) g =
  ([(True, p), (False, (-) 1 p)], g)
{-
sampleWeighted (UniformEnum (b0, b1)) g =
  let p = (/) 1 (fromIntegral ((fromEnum b1) - (fromEnum b0) + 1))
      samples = map (\x-> (x, p)) [b0..b1]
   in (samples, g)
sampleWeighted (Flip p) g =
  ([(True, p), (False, (-) 1 p)], g)
-}
--Default: just attempt to produce an ordinary sample
sampleWeighted d g0 =
  let (v, g1) = trySample d g0
      v' = fromMaybe [] (fmap (\x -> [(x, 1)]) v)
   in (v', g1)
--  let (g1, g2) = split g0
--   in

--TODO returning the generator is a bit strange, seeing as a lot of lazy computation is required to get it. 
expectationWeightedSampleQuotient :: (Rand a) -> (a -> Prob) -> StdGen -> ([(Prob, Prob)], StdGen)
expectationWeightedSampleQuotient d f g0 =
  let (stream, g1) = sampleWeighted d g0
      processStream [] (n, d) = [(n, d)]
      processStream ((v, p):s) (n, d) =
        let q' = ((+) n ((*) p (f v)), (+) d p)
         in (:) q' (processStream s q')
   in (processStream stream (0, 0), g1)

--TODO need to make sure sampleWeighted never produces 0s.
expectationWeightedSample :: (Rand a) -> (a -> Prob) -> StdGen -> ([Prob], StdGen)
expectationWeightedSample r f g0 =
  let (s, g1) = expectationWeightedSampleQuotient r f g0
   in (map (uncurry (/)) s, g1)

--Invariant Checking
checkInvariant :: Rand a -> Bool
checkInvariant (WeightedCMF []) = True
checkInvariant (WeightedCMF [(a, p)]) = (==) p 1
checkInvariant (WeightedCMF l) =
  let (lv, lp) = last l
      monotonic = all (uncurry (<=)) (adjacentPairs (map snd l))
   in (&&) ((==) 1 lp) monotonic
--checkInvariant (WeightedCMF ((_, p0):(x, p1):r)) = (&&) ((<=) p0 p1) (checkInvariant (WeightedCMF (x, p1):r))
checkInvariant (UniformEnum (a, b)) = (<=) (fromEnum a) (fromEnum b)

toCumulative :: [Prob] -> [Prob]
toCumulative l =
  let toCumulative' [] _ = []
      toCumulative' (a:l) c =
        let ac = (+) a c
         in ac:(toCumulative' l ac)
   in toCumulative' l 0

fromCumulative :: [Prob] -> [Prob]
fromCumulative [] = []
fromCumulative [a] = [a]
fromCumulative l = 
  let fromCumulative' :: [Prob] -> [Prob]
      fromCumulative' [a,b] = [(-) b a]
      fromCumulative' (a:b:l) = ((-) b a):(fromCumulative' (b:l))
   in (:) (head l) (fromCumulative' l)

--Convert cumulative values to masses.
fromCumulativeValues :: [(a, Prob)] -> [(a, Prob)]
fromCumulativeValues l =
  let vals = map fst l
      cums = map snd l
      masses = fromCumulative cums
   in zip vals masses

--Experimental optimizer.  It may produce incorrect values, and doesn't do much.
optimizeDist :: (Rand a) -> (Rand a)
--No optimizations for return.
optimizeDist (Return r) = Return r
--Uniform optimizations:
optimizeDist (Uniform [a]) = Return a
optimizeDist (WeightedCMF [(a, p)]) = Return a
--Application of the Law of Monad.
--optimizeDist (Bind d Return) = optimizeDist d
--Direct Application of Conditioning (Removal of a layer of laziness).
{-
optimizeDist (Condition (Uniform a) f) = --TODO need to reweight.
  let possible = filter f a
   in if (==) [] possible then Uniform a else Uniform possible
optimizeDist (Condition (WeightedCMF a) f) = --TODO need to reweight.
  let masses = zip (map fst a) (fromCumulative (map snd a))
      possible = filter (f . fst) a
      possibleMass = sum (map snd possible)
      rescaledMasses = map (\x -> (/) x possibleMass) (map snd possible)
   in if (==) 0 possibleMass then (WeightedCMF a) else zip (map fst possible) rescaledMasses
-}
--Checking for Conditioning Shortcuts
--TODO want to pattern match on functions.  I don't know if such madness is permitted in Haskell.
--optimizeDist (Condition d (const True)) = d
--optimizeDist (Condition d (const False)) = d
--Finite enumerations
optimizeDist u@(UniformEnum (a, b)) =
  if ((==) a b) then Return a else u
--TODO use support to check for all true / all false conditions.

--Don't use this function, it probably doesn't do what you want.
prob :: (Eq a) => Rand a -> a -> Prob
prob (Uniform u) v = if elem v u then ((/) 1.0 (fromIntegral (length u))) else 0 --TODO doesn't handle the case where something is entered multiple times.
prob (UniformEnum (a, b)) v =
  let iv = fromEnum v
      ia = fromEnum a
      ib = fromEnum b
   in if (&&) ((>=) iv ia) ((<=) iv ib) then (/) 1.0 (fromIntegral ((-) ib ia)) else 0
prob (UniformReal (b0, b1)) v
  | (&&) (v >= b0) (v <= b1) = realToFrac (1 / (b1 - b0))
  | otherwise = 0
prob (Gaussian mu sigma) v =
  let (muf, sigmaf, vf) = (realToFrac mu, realToFrac sigma, realToFrac v)
   in (1 / (sigmaf * (sqrt (2 * pi)))) * (exp (-(vf - muf)^2 / (2 * sigmaf^2)))
prob (Exponential lambda) v
  | (<) 0 v = 0
  | otherwise = 
    let (lambdaf, vf) = (realToFrac lambda, realToFrac v)
     in exp (-((*) lambdaf vf))
--TODO more cases of this function.


--These functions are pretty experimental at this point.
support :: (Rand a) -> [a]
support = undefined

--Obtain the maximum likelihood value of this distribution.
maximumLikelihood :: (Rand a) -> [(a, Prob)]
maximumLikelihood = undefined

--Obtain the Bayesian estimate for the value of this distribution.
bayesianValueInference :: (Rand a) -> (Rand a) -> [(a, Prob)]
bayesianValueInference = undefined
--bayesianValueInference = IndependentJointDistribution a b

--Sampled approximations of a distribution.
sampleDistToDist :: (Rand a) -> StdGen -> [(Rand a)]
sampleDistToDist = undefined
--sampleDistToDist = heads

sampleDiscardGen :: (Rand a) -> StdGen -> a
sampleDiscardGen a g = fst (sample a g)

replaceIndexed :: Int -> a -> [a] -> [a]
replaceIndexed i v l = (take i l) ++ ((:) v (drop (succ i) l))

selectRandom :: [a] -> Rand a
selectRandom [] = undefined
selectRandom l@(a:r) =
  let len = length l
   in do i <- (Uniform [0..(pred len)])
         return ((!!) l i)


--TODO very slow draft implementation (O(n^2)).  Could be faster if less time was spent traversing lists.
--Try implementing production of random integer lists and mapping the list onto the input.
shuffleList :: [a] -> Rand [a]
shuffleList [] = Return []
shuffleList [a] = Return [a]
shuffleList l@(v:r) =
  let len = length l
   in do i <- (UniformEnum (0, pred len)) ;
         if (==) 0 i
         then fmap ((:) v) (shuffleList r) 
         else fmap ((:) ((!!) l i)) (shuffleList (replaceIndexed (pred i) v r))


--TODO store probabilities in arrays.
data MarkovModel a = MarkovModel (a -> Rand a)
--data MarkovModel a = 

data HiddenMarkovModel a b = HiddenMarkovModel (MarkovModel a) (a -> Rand b)

--Finite versions:
mmRand :: MarkovModel a -> a -> Int -> Rand [a]
mmRand mm@(MarkovModel f) s0 0 = Return []
mmRand mm@(MarkovModel f) s0 count =
  do next <- (f s0)
     fmap ((:) s0) (mmRand mm next (pred count))

hmmRand :: HiddenMarkovModel a b -> a -> Int -> Rand [(a, b)]
hmmRand (HiddenMarkovModel mm em) s0 count =
  do hiddenStates <- mmRand mm s0 count
     emmisions <- mapM em hiddenStates
     Return (zip hiddenStates emmisions)

--Infinite versions: (Require a change to sample Bind to take finite samples from them.)
{-
mmRand :: MarkovModel a -> a -> Rand [a]
mmRand mm@(MarkovModel f) s0 =
  do next <- (f s0)
     fmap ((:) s0) (mmRand mm next)

hmmRand :: HiddenMarkovModel a b -> a -> Rand [(a, b)]
hmmRand (HiddenMarkovModel mm em) s0 =
  do hiddenStates <- mmRand mm s0
     emmisions <- mapM em hiddenStates
     Return (zip hiddenStates emmisions)
-}
normalizeCumulativeDensities :: [Prob] -> [Prob]
normalizeCumulativeDensities l =
  let m = maximum l
   in map (\x -> x / m) l


--Randomizers.  These allow us to generate random distributions of things.

class Randomizable a where
  randomize :: a -> Rand (Rand a)

class SizedRandomizable a where
  randomizeSized :: Int -> Rand (Rand a)

--Draw uniformly from the set of distributions over the elements of [a].
randomDistribution :: [a] -> Rand (Rand a)
randomDistribution values =
  do cumDensities <- mapM (const (UniformReal (0, 1))) values
     let normCumDensities = normalizeCumulativeDensities cumDensities
         paired = zip values normCumDensities
         sorted = sortWith snd paired
      in return $ WeightedCMF sorted

randomSplit :: [a] -> Rand ([a], [a])
randomSplit a =
  do shuffled <- shuffleList a ;
     splitIdx <- UniformEnum (0, (pred (length a))) ;
     return (splitAt splitIdx shuffled) ;


--Probability distribution statistics and comparisons.
entropy :: Rand a -> Prob
entropy (Uniform u) = log (fromIntegral (length u))
entropy _ = undefined --TODO

--Kullback-Liebler Divergence
-- D_{KL} (P||Q)
klDiscreteList :: (Eq a) => [(a, Prob)] -> [(a, Prob)] -> Maybe Prob
klDiscreteList [] _ = Just 0
klDiscreteList opl@((p, pp):pl) oql@((q, pq):ql)
 | p == q = liftM2 (+) (Just $ pp * (log (pp / pq))) (klDiscreteList pl ql)
 | otherwise = klDiscreteList opl ql --This q entry has no p entry.  This means pp = 0, so we skip the term.
klDiscreteList _ _ = undefined --If there are p left, but no q, we divide by 0, and KL divergence is undefined.

avg2 :: (Num a, Fractional a) => a -> a -> a
avg2 a b = (a + b) / 2

symKlDiscreteList :: (Eq a) => [(a, Prob)] -> [(a, Prob)] -> Maybe Prob
symKlDiscreteList p q = liftM2 avg2 (klDiscreteList p q) (klDiscreteList q p)

symKlDiscrete :: Rand a -> Rand a -> Maybe Prob
symKlDiscrete (WeightedOrd m0) (WeightedOrd m1) =
  let mTup = (m0, m1)
      (l0, l1) = mapT2 Data.Map.toList mTup
   in symKlDiscreteList l0 l1

--Jensen-Shannon divergences.
jensenShannonList :: (Eq a, Ord a) => [(a, Prob)] -> [(a, Prob)] -> Prob
jensenShannonList p q =
  let grouped = groupWith fst $ sort (p ++ q) --TODO a merge function would be much faster.
      m = map (\ l -> ((fst . head) l, (sum (map snd l)) / 2)) grouped
   in avg2 (fromJust $ klDiscreteList p m) (fromJust $ klDiscreteList q m)

jsDiscrete :: Rand a -> Rand a -> Prob
jsDiscrete (WeightedOrd m0) (WeightedOrd m1) =
  let mTup = (m0, m1)
      l@(l0, l1) = mapT2 Data.Map.toList mTup
   in jensenShannonList l0 l1



--Very poor metric for distribution difference, with many undesirable properties.
euclideanDistanceListSquared :: (Num n) => [n] -> [n] -> n
euclideanDistanceListSquared a b = sum (map (\ (a, b) -> (a - b) ^ 2) (zip a b))

euclideanDistanceList :: [Prob] -> [Prob] -> Prob
euclideanDistanceList a b = sqrt (euclideanDistanceListSquared a b)

--Weighted square error, weighted by the first element
--weightedSquareErrorList :: [a] -> Rand a -> Rand a -> Prob
--weightedSquareErrorList a b

sqrtSqrtListSimilarity :: (Floating n) => [n] -> [n] -> n
sqrtSqrtListSimilarity a b = sum (map (\ (a, b) -> (sqrt a) * (sqrt b)) (zip a b))

--TODO coefficient of determination.
-- KS statistic.

--TODO this is horribly inefficient: there is an O(a + b) algorithm, but this is O(ab)
auroc :: (Ord o) => [o] -> [o] -> Prob
auroc a b = 
  let pairs = cartesianProduct a b
      indicators :: [Int]
      indicators = map (\ (a, b) -> if a < b then 1 else 0) pairs
   in (fromIntegral $ sum indicators) / (fromIntegral $ length pairs)


{-
auroc a b =
  let atl = length a
      btl = length b
      tl = atl + btl
      auroc' p _ _ = 
      auroc' _ (a:al, ac) (_, bc) = 
      auroc' p (a:al, ac) (b:bl, bc)
       | a < b = (a - p) * ( ) / 2 + (auroc' a:al)
       | otherwise =
      
      
  let a' = map (\ x -> (x, -1)) a
      b' = map (\ x -> (x, 1)) b
  let mixed = sortWith snd ((map ((,) -1) a)
-}

