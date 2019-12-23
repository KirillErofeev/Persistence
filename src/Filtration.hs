{-#language MultiParamTypeClasses #-}
module Filtration where

import Numeric.Field.Fraction

import Data.List (foldl', sort)
import qualified Data.Set as Set
import Data.Monoid
import Data.Semigroup
import Data.Maybe (isJust, fromJust)

import Types
import Instances
import Homology
import FiniteFields

import Debug.Trace 

instance Bounded Double where
    maxBound = 2^1023
    minBound = -2^1023

class MetricSpace a where
    distance :: a -> a -> Double

instance RealFloat a => MetricSpace (Point a) where
  distance (Point a b) (Point a0 b0) = realToFrac $ sqrt $ (a - a0)^2 + (b - b0)^2

instance MetricSpace Double where
  distance a b = abs $ a - b

--class (Ord f) => MetricSpace f a where
--    distance :: a -> a -> f 
--
--instance RealFloat a => MetricSpace Double (Point a) where
--  distance (Point a b) (Point a0 b0) = realToFrac $ sqrt $ (a - a0)^2 + (b - b0)^2
--
--instance MetricSpace Double Double where
--  distance a b = abs $ a - b

buildFiltration points maxDim maxDist = concat $ snd $
    foldl' f (zeroDimSimplicies, [zeroDimSimplicies]) [1..maxDim-1] where
        zeroDimSimplicies = --foldl' (<>) mempty $
            pure DSimplex <*> (expand emptySimplex <$> points) <*> pure 0.0
        f (curDimSimplicies, simplicies) dim =
            let newSimplicies = addNewDim curDimSimplicies
            in (newSimplicies, newSimplicies:simplicies)
        addNewDim baseSimplicies = replaysFilter $ (fromJust <$>) $
            filter isJust $ concat $ findNewSimplicies <$> baseSimplicies
        findNewSimplicies simplex = addSimplex simplex <$> 
            filter (not . flip elem (dSimplex simplex)) points
        addSimplex (DSimplex simplex degree) point 
            | maxNewDistance <= maxDist = Just $ DSimplex (expand simplex point) $
                max (maxNewDistance) degree
            | otherwise                 = Nothing where
                maxNewDistance = maxNewDistances simplex point -- AWKWARD
        maxNewDistances simplex point = getMax (foldMap (Max . (distance point)) simplex)

buildFiltration0 distance points maxDim maxDist = concat $ snd $
    foldl' f (zeroDimSimplicies, [zeroDimSimplicies]) [1..maxDim-1] where
        zeroDimSimplicies = --foldl' (<>) mempty $
            pure DSimplex <*> (expand emptySimplex <$> points) <*> pure 0.0
        f (curDimSimplicies, simplicies) dim =
            let newSimplicies = addNewDim curDimSimplicies
            in (newSimplicies, newSimplicies:simplicies)
        addNewDim baseSimplicies = replaysFilter $ (fromJust <$>) $
            filter isJust $ concat $ findNewSimplicies <$> baseSimplicies
        findNewSimplicies simplex = addSimplex simplex <$> 
            filter (not . flip elem (dSimplex simplex)) points
        addSimplex (DSimplex simplex degree) point 
            | maxNewDistance <= maxDist = Just $ DSimplex (expand simplex point) $
                max (maxNewDistance) degree
            | otherwise                 = Nothing where
                maxNewDistance = maxNewDistances simplex point -- AWKWARD
        maxNewDistances simplex point = getMax (foldMap (Max . (distance point)) simplex)

replaysFilter ss = (Set.toList . Set.fromList) $ sortSimplex <$> ss where
    sortSimplex (DSimplex (ListSimplex i s) d) = DSimplex (ListSimplex i (sort s)) d
--replaysFilter = undefined

testF = goodShow $ (buildFiltration [1,2,7] 5 1000 :: [DSimplex ListSimplex Double])
testF0 = goodShow $ (buildFiltration0 ((abs .) . (-)) [1,2,7] 5 1000 :: [DSimplex ListSimplex Double])

goodShow s = foldMap ((++ "\n") . goodShow') (sort s)
goodShow' (DSimplex (ListSimplex _ s) d) = show s ++ "->" ++ show d

points = [1,2,3]

data PointS = PointS Double Double String deriving (Eq, Ord)

instance MetricSpace PointS where
  distance (PointS a b _) (PointS a0 b0 _) = realToFrac $ sqrt $ (a - a0)^2 + (b - b0)^2

instance Show PointS where
    show (PointS x y s) = s

circlePointCloud n r = circleP 0 0 r <$> twoPi n
--circleP :: Double -> Double -> Double -> Double -> Point Double
circleP a b r x = Point (a + r * cos x) (b + r * sin x) --(take 7 $ show (x - pi))
twoPi n = [0,2*pi/(n-1)..2*pi]

circle2PointCloud n r = (circleP 30 0 r <$> twoPi n) <> (circleP 0 0 r <$> twoPi n)

slowFiltration n = circle2Filtration 23 1 4 7
slowHomology n = intervalsMultiplicity $ filtEmptyIntervals $ computePersistentHomology Z2Unit $ circleFiltration 17 1 4 7
circleFiltration n r maxDim maxDist  = buildFiltration (circlePointCloud n r) maxDim maxDist
circle2Filtration n r maxDim maxDist = buildFiltration (circle2PointCloud n r) maxDim maxDist
circleHomology n r maxDim maxDist = intervalsMultiplicity $ filtEmptyIntervals $ computePersistentHomology Z2Unit $ circleFiltration n r maxDim maxDist
--circleHomology n r maxDim maxDist = computePersistentHomology (1%1 :: Fraction Integer) $ reverse $ circleFiltration n r maxDim maxDist
circle2Homology n r maxDim maxDist = computePersistentHomology (1%1 :: Fraction Integer) $ circle2Filtration n r maxDim maxDist

line2F :: Integer -> Double -> [DSimplex ListSimplex Double]
line2F maxDim maxDist = buildFiltration [0,1,2,7] maxDim maxDist
line2H maxDim maxDist = computePersistentHomology (1%1 :: Fraction Integer) (line2F maxDim maxDist)
line2HZ2 maxDim maxDist = computePersistentHomology Z2Unit (line2F maxDim maxDist)
--line maxDim maxDist = computePersistentHomology (1%1 :: Fraction Integer) 
line maxDim maxDist = computePersistentHomology Z2Unit
   (reverse $ buildFiltration [1,7,9] maxDim maxDist :: [DSimplex ListSimplex Double])

filtEmptyIntervals (ListsPIntervals pis) = ListsPIntervals $ filter intervalNotEmpty <$> pis where
    intervalNotEmpty (PIntervalFinite l r) | r - l < 1e-11 = False
    intervalNotEmpty _                                     = True

intervalsMultiplicity (ListsPIntervals pis) = PIntOverZ $ foldr im [] <$> pis where
    im pi []              = [(1,pi)] 
    im pi pisOZ@((piZ,pi1):pis) | isClose pi pi1 = (piZ+1,pi1):pis
                                | otherwise = (1,pi):pisOZ
    isClose (PIntervalFinite l r) (PIntervalFinite l0 r0) 
       | abs (l - l0) < 1e-7 && abs (r - r0) < 1e-7 = True
    isClose (PIntervalInfinite l) (PIntervalInfinite l0) 
       | abs (l - l0) < 1e-7 = True
    isClose _ _ = False
-- 100 1 3 2   -> 166k simplexes 60 sec
-- 100 1 3 1   -> 13k  simplexes 5 sec
-- 100 1 4 1   -> 72k  simplexes 50 sec
-- 100 1 4 0.3 -> 1.5k  simplexes 2 sec
