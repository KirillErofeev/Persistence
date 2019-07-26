--{-#language MultiParamTypeClasses #-}
{-#language RankNTypes #-}
{-#language FlexibleInstances #-}
{-#language FlexibleContexts #-}
{-#language NoMonomorphismRestriction #-}

module Homology where

import qualified Data.Map.Strict as Map
import Data.List

import qualified Data.PartialOrd as PO

import Types

instance Eq a => PO.PartialOrd (ListSimplex a) where
  compare (ListSimplex a) (ListSimplex b)
      | length a == length b && subsetList a b = Just EQ
      | length a <  length b && subsetList a b = Just LT
      | length a >  length b && subsetList b a = Just GT
      | otherwise                              = Nothing
          where
              subsetList a b = foldr ((&&) . flip elem b) True a

  a <= b = PO.compare a b == Just LT || PO.compare a b == Just EQ

instance Simplex ListSimplex where
  dimension       (ListSimplex l) = length l - 1
  simplexToList   (ListSimplex l) = l
  simplexFromList = ListSimplex

instance Simplex s => Simplex (DSimplex s) where
  dimension     (DSimplex l d) = dimension     l
  simplexToList (DSimplex l d) = simplexToList l
  simplexFromList l            = DSimplex (simplexFromList l) 0

instance {-#OVERLAPPABLE#-} (Ord a, FSimplex s) => Ord (s a) where
  compare a b | degree    a /= degree    b = compare (degree    a) (degree    b)
              | dimension a /= dimension b = compare (dimension b) (dimension a)
              | otherwise                  = compare a b

instance {-#OVERLAPPABLE#-} (Eq a, Simplex s) => Eq (s a) where
  l == l0 | dimension l /= dimension l0 = False
          | otherwise                  = all (uncurry (==) ) $ zip (simplexToList l) (simplexToList l0)
  --l == l0 = simplexToList l == simplexToList l0
  --a == b = True


instance Filtration ListFiltration where
   emptyFiltration = ListFiltration []
   addSimplex (ListFiltration l) s = ListFiltration (s:l)
   updDegrees f (ListFiltration l) = ListFiltration $ (updDegree f) <$> l
   --toListSimpleces (ListFiltration l) = l

instance Simplex a => FSimplex (DSimplex a) where
  fsimplex l d = DSimplex (simplexFromList l) d
  degree = dSimplex
  updDegree f s = s {dSimplex = f $ dSimplex s}



class Metric a where
  distance :: a -> a -> Double


instance RealFloat a => Metric (Point a) where
  distance (Point a b) (Point a0 b0) = realToFrac $ sqrt $ (a - a0)^2 + (b - b0)^2

instance Metric Double where
  distance a b = abs $ a - b


instance Show a => Show (Point a) where
  show (Point x y) = show x ++ " " ++ show y

instance Show a => Show (ListSimplex a) where
  show (ListSimplex s) = show s

instance (Show a, Show (s a)) => Show (DSimplex s a) where
  show (DSimplex s a) = show s ++ "^" ++ show a

instance (FSimplex (f s), Show a, Show (s a), Show (f s a)) => Show (ListFiltration (f s) a) where
  show (ListFiltration s) = showByDegree $ filter ((>0) . length) $ map getSimplecesByDegree [0..maxDegree] where
    maxDegree = foldr (max . degree) 0 s
    getSimplecesByDegree d = filter ((==d) . degree) s
    showByDegree (s'@(l:ls):lss) = "Degree " ++ show (degree l)            ++ ":\n"
                                            ++ show (simplexToList <$> s') ++ " \n"
                                            ++ showByDegree lss
    showByDegree [] = ""

--metricFiltration :: [Double] -> [Point Double] -> ListFiltration (DSimplex ListSimplex) Int
--metricFiltration dists pointCloud = foldl' addNewGrade emptyFiltration pds where
--  pds = toPairs dists
--  addNewGrade filtration interval = foldr (flip addSimplex . (\(a,b) -> fsimplex [a,b] 0)) (incDegrees filtration) (newSimplexes interval)
--  pcDists = distances pointCloud
--  newNeighbors (s,f) = fst <$> filter (\(_,d) -> d >= s && d < f) pcDists
--  neighborsToSimplices filtration neighbors = neighborToSimplices filtration <$> neighbors
--  neighborToSimplices  filtration (a,b) | a == b = fsimplex [a] 0
--                                        | True   = fsimplex [a] 0


metricFiltration' dists pointCloud = foldl' addNewGrade emptyFiltration pds where
  pds = toPairs dists
  --addNewGrade filtration interval = foldr (flip addSimplex . (\(a,b) -> DSimplex (ListSimplex [a,b]) 0)) (incDegrees filtration) (newSimplexes interval)
  addNewGrade filtration interval = foldr (flip addSimplex . (\(a,b) -> fsimplex [a,b] 0)) (incDegrees filtration) (newSimplexes interval)
  pcDists = distances pointCloud
  newSimplexes (s,f) = fst <$> filter (\(_,d) -> d >= s && d < f) pcDists

toPairs (x:y:xs) = (x,y) : toPairs (y:xs)
toPairs xs       = []

distances pointCloud = --Map.fromList $
  commutativeApp dictDistance ([], pointCloud) where
    dictDistance a b = ((a,b), distance a b)

commutativeApp f (ac, x:xs) = commutativeApp f ((f x <$> x:xs) ++ ac, xs)
commutativeApp f (ac, []  ) = ac

test :: ListFiltration (DSimplex ListSimplex) (Point Double)
test = metricFiltration' [0,1,3,100] [Point 0 0, Point 2 2, Point 1 1, Point 3 3] 
