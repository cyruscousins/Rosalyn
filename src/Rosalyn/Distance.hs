module Rosalyn.Distance where

import Data.Tuple
import Data.List
import Data.Array.Unboxed

import Rosalyn.ListUtils

--Efficient representations of strictly evaluated distance matrices.

--TODO make generic in distance type.  
data DistanceMatrix = DistanceMatrix Int (UArray Int Double)

matrixIndex :: Int -> (Int, Int) -> Int
matrixIndex s (i, j)
 | (i < j) = matrixIndex s (j, i)
 | otherwise = (i - j - 1) + (sum [(s - j)..(s - 1)])

{-
-012
0-34
13-5
245-
-}

--TODO
matrixIndexToCoords :: Int -> Int -> (Int, Int)
matrixIndexToCoords s i
 | (i < s) = (i + 1, 0)
 | otherwise = matrixIndexToCoords (s - 1) (i - s)

distance :: DistanceMatrix -> Int -> Int -> Double
distance d@(DistanceMatrix size dm) i j
  | (i < 0 || i >= size || j < 0 || j >= size) = undefined
  | (j == i) = 0
  | otherwise = (!) dm (matrixIndex size (i, j))

meanDistance :: DistanceMatrix -> Double
meanDistance d@(DistanceMatrix size dm) = (2 * (sum (elems dm))) / (realToFrac $ size * size)

--A normalized distance matrix has mean distance 1.
normalizeDistanceMatrix :: DistanceMatrix -> DistanceMatrix
normalizeDistanceMatrix d@(DistanceMatrix size dm) =
  let m = meanDistance d
      im = 1 / m
      rescaled = amap ((*) im) dm
   in DistanceMatrix size rescaled

instance Show DistanceMatrix where
  show d@(DistanceMatrix size _) = 
    let rows = map (\ i -> intercalate "\t" (map (\ j -> show $ distance d i j) [0..(pred size)])) [0..(pred size)]
     in intercalate "\n" rows

isValidMatrix :: DistanceMatrix -> Bool
isValidMatrix (DistanceMatrix size arr) = True

isNonNegative :: DistanceMatrix -> Bool
isNonNegative (DistanceMatrix size arr) = True --TODO

epsilon :: Double
epsilon = 0.0000000001

checkTriangleInequality :: DistanceMatrix -> Bool
checkTriangleInequality dm@(DistanceMatrix size _) =
  let triples = allTriples [0..(pred size)] --TODO inefficient: could exploit symmetry.
      df = distance dm
   in all (\ (a, b, c) -> (df a b) + (df b c) + epsilon >= (df a c)) triples

--A matrix has addivity degree 0 iff it is additive.
additivityDegree :: DistanceMatrix -> Double
additivityDegree dm@(DistanceMatrix size _) =
  let quads = allQuadrouples [0..(pred size)]
      df = distance dm
      degree (a, b, c, d) =
        let dab = df a b
            dcd = df c d
            dac = df a c
            dbd = df b d
            dad = df a d
            dbc = df b c
         in max 0 (dab + dcd - (max (dac + dbd) (dad + dbc)) - epsilon)
   in sum (map degree quads)

checkFourPoint :: DistanceMatrix -> Bool
checkFourPoint dm@(DistanceMatrix size _) =
  let quads = allQuadrouples [0..(pred size)]
      df = distance dm
      check (a, b, c, d) =
        let dab = df a b
            dcd = df c d
            dac = df a c
            dbd = df b d
            dad = df a d
            dbc = df b c
         in dab + dcd <= epsilon + (max (dac + dbd) (dad + dbc))
   in all check quads

checkAdditivity :: DistanceMatrix -> Bool
checkAdditivity = checkFourPoint

isDistanceMatrix :: DistanceMatrix -> Bool
isDistanceMatrix dm = (isValidMatrix dm) &&  (checkTriangleInequality dm)

isAdditiveMatrix :: DistanceMatrix -> Bool
isAdditiveMatrix dm = (isValidMatrix dm) && (checkAdditivity dm)
--TODO confirm that additivity implies triangle inequality.

--------------------------
--Distance matrix creation

createDistanceMatrixFromIndexed :: Int -> [(a, Int)] -> (a -> a -> Double) -> DistanceMatrix
createDistanceMatrixFromIndexed size l df =
  let pairs = allPairsUnordered l
      pairToArrDat ((a0, i0), (a1, i1)) = (matrixIndex size (i1, i0), df a0 a1)
      arrDat = map pairToArrDat pairs
      arrSize = div ((size - 1) * (size - 0)) 2
      arr = array (0, (pred arrSize)) arrDat
   in DistanceMatrix size arr

createDistanceMatrix :: [a] -> (a -> a -> Double) -> DistanceMatrix
createDistanceMatrix a = createDistanceMatrixFromIndexed (length a) (map swap $ indexList a)

----------------------------
--Distance matrix operations

euclideanDistanceSquaredDM :: DistanceMatrix -> DistanceMatrix -> Double
euclideanDistanceSquaredDM dm0@(DistanceMatrix s0 a0) dm1@(DistanceMatrix s1 a1)
 | s0 /= s1 = undefined
 | otherwise = 2 * (sum (map (\i -> (((!) a0 i) - ((!) a1 i)) ^ 2) [0..(pred s0)]))

euclideanDistanceDM :: DistanceMatrix -> DistanceMatrix -> Double
euclideanDistanceDM a b = sqrt (euclideanDistanceSquaredDM a b)

euclideanDistanceNormalizedDM :: DistanceMatrix -> DistanceMatrix -> Double
euclideanDistanceNormalizedDM a b = euclideanDistanceDM (normalizeDistanceMatrix a) (normalizeDistanceMatrix b)

---------------------
--Distance operations

lSquaredDistance :: (Num n, Integral i) => i -> [n] -> [n] -> n
lSquaredDistance i (a:al) (b:bl) = (+) ((a - b) ^ 2) (lSquaredDistance i al bl)
lSquaredDistance _ _ _ = 0

lDistance :: (Floating f, Integral i) => i -> [f] -> [f] -> f
lDistance i a b = sqrt $ lSquaredDistance i a b

