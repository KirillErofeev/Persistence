--{-#language MultiParamTypeClasses #-}
{-#language RankNTypes #-}
{-#language FlexibleInstances #-}
{-#language FlexibleContexts #-}
{-#language NoMonomorphismRestriction #-}
{-#language NamedFieldPuns#-}

module Homology where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe (fromJust, isJust)
import Data.Group

import Control.Applicative (ZipList(..))

import qualified Data.PartialOrd as PO

import Types

subsetList a b = foldr ((&&) . flip elem b) True a

instance Eq a => Eq (ListSimplex a) where
   (ListSimplex i a) == (ListSimplex i0 a0) =
      i == i0 && length a == length a0 && a `subsetList` a0

instance Eq a => PO.PartialOrd (ListSimplex a) where
  compare (ListSimplex i a) (ListSimplex i' b)
      | i        /= i'                         = Nothing
      | length a == length b && subsetList a b = Just EQ
      | length a <  length b && subsetList a b = Just LT
      | length a >  length b && subsetList b a = Just GT
      | otherwise                              = Nothing

  a <= b = PO.compare a b == Just LT || PO.compare a b == Just EQ

instance Simplex ListSimplex where
  emptySimplex                      = ListSimplex []
  simplexAppend (ListSimplex s0 ) (ListSimplex s1) = sort
  dimension     (ListSimplex i l) = length l - 0
  simplexToList (ListSimplex i l) = l
  simplexFromList = ListSimplex False 
  isInverse (ListSimplex i _) = i
  inverse   (ListSimplex i s) = ListSimplex (not i) s
  boundary  (ListSimplex i s) = zipWith ListSimplex (iterate not i) (boundary' s) where
      boundary' []     = []
      boundary' (s:ss) = ss : (map (s:) $ boundary' ss)
  --boundary  (ListSimplex i s) = (snd . fst) $ foldr boundaryFoldr ((s, []), False) s where
  --    boundaryFoldr simplex ((simplicies, boundaryOut), isInverseSign) =
  --       ((simplicies
  --        ,ListSimplex (i /= isInverseSign) (filter (/=simplex) simplicies) : boundaryOut)
  --         ,not isInverseSign)

instance Simplex s => Simplex (DSimplex s) where
  emptySimplex                 = DSimplex [] 0
  dimension     (DSimplex l d) = dimension     l
  simplexToList (DSimplex l d) = simplexToList l
  simplexFromList l            = DSimplex (simplexFromList l) 0
  inverse       (DSimplex l d) = DSimplex (inverse l) d
  isInverse     (DSimplex l d) = isInverse l
  boundary      (DSimplex l d) = flip DSimplex d <$> (boundary l) 

instance {-#OVERLAPPABLE#-} (Ord a, FSimplex s) => Ord (s a) where
  compare a b | degree    a /= degree    b = compare (degree    a) (degree    b)
              | dimension a /= dimension b = compare (dimension b) (dimension a)
              | otherwise                  = compare a b

instance {-#OVERLAPPABLE#-} (Eq a, Simplex s) => Eq (s a) where
  l == l0 | dimension l /= dimension l0 = False
          | otherwise                  = all (uncurry (==) ) $ zip (simplexToList l) (simplexToList l0)
  --l == l0 = simplexToList l == simplexToList l0
  --a == b = True

instance Foldable ListFiltration where
   foldMap fm (ListFiltration f) = foldMap fm f

instance Filtration ListFiltration where
   emptyFiltration = ListFiltration []
   addSimplex (ListFiltration l) s = ListFiltration (s:l)
   updDegrees f (ListFiltration l) = ListFiltration $ (updDegree f) <$> l
   toListSimplices (ListFiltration l) = l

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
  show (ListSimplex i s) = show i ++ show s

instance (Show a, Show (s a)) => Show (DSimplex s a) where
  show (DSimplex s a) = show s ++ "^" ++ show a

instance (FSimplex s, Show a, Show (s a)) => Show (ListFiltration (s a)) where
  show (ListFiltration s) = showByDegree $ filter ((>0) . length) $ map getSimplecesByDegree [0..maxDegree] where
    maxDegree = foldr (max . degree) 0 s
    getSimplecesByDegree d = filter ((==d) . degree) s
    showByDegree (s'@(l:ls):lss) = "Degree " ++ show (degree l)             ++ ":\n"
                                             ++ show (simplexToList <$> s') ++ " \n"
                                             ++ showByDegree lss
    showByDegree [] = ""

instance PIntervals ListsPIntervals where
    addInterval (ListsPIntervals intervals) dimension pInterval = ListsPIntervals $ snd $
                         foldr add (dimension, []) intervals where
        add iHomology (kDim, intervals')
            | kDim <= 0 = (0       , (pInterval : iHomology) : intervals')
            | otherwise = (kDim - 1,              iHomology  : intervals')
        fitMdHomology
            | length intervals > dimension = intervals
            | otherwise                     = intervals ++ (take dimensionsDif $ repeat [])
        dimensionsDif = (dimension - length intervals + 1)

    listKIntervals (ListsPIntervals p) k | k < length p = p !! k
                                         | otherwise    = []
    listIntervals  = getListsPIntervals
    emptyPIntervals = ListsPIntervals []


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

test :: ListFiltration ((DSimplex ListSimplex) (Point Double))
test = metricFiltration' [0,1,3,100] [Point 0 0, Point 2 2, Point 1 1, Point 3 3] 

instance Eq s => Eq (T_ s) where
   t0 == t1 = tElemSimplex_ t0 == tElemSimplex_ t1

-- https://geometry.stanford.edu/pape_rs/z-cph-05/zc-cph-05.pdf
-- should use some map tree instead of list of T_
--computePersistentHomology :: (Filtration f, FSimplex s, PIntervals h) => f (s a) -> h b
computePersistentHomology filtration = foldr undefined (tArray, pIntervals) where
    tArray = getZipList $
       (ZipList $ T_ <$> listF)     <*> ZipList (repeat False)   <*>
       (ZipList $ degree <$> listF) <*> ZipList (repeat Nothing)
    listF = foldr (:) [] filtration
    pIntervals = emptyPIntervals :: ListsPIntervals Int
    removePivotRows t simplex = head $ dropWhile fst $
       iterate removePivotRows' $ (True, (filter isMarked $ boundary simplex))
    removePivotRows' (p,[]) = (False, [])
    removePivotRows' (p,ss) 
       | isEmpty . simplex . maxIndex $ ss = (False, ss)
       | otherwise = (False, )
    isEmpty = isJust . tElemBoundary_ . findSimplex
    isMarked = tElemIsMarked_ . findSimplex
    maxIndex boundary = maximum $ zipWith DSimplex boundary $ findDegree <$> boundary 
    findDegree = tElemDegree_ . findSimplex 
    findSimplex simplex = fromJust $
       find (\s -> (reverseIfInverse . tElemSimplex_) s == simplex) tArray
    reverseIfInverse s | isInverse s = inverse s
                       | otherwise   = s
