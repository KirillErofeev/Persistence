{-#language FlexibleInstances #-}
{-#language FlexibleContexts #-}
module Instances where

import qualified Data.PartialOrd as PO
import Data.List
import Data.Group

import Types 

import Debug.Trace

instance (Simplex s, Eq (s a), Eq a) => Eq (T_ (s a)) where
   t0 == t1 =
      tElemSimplex_ t0 == tElemSimplex_ t1
       || inverse (tElemSimplex_ t0) == tElemSimplex_ t1 

instance (Ord a, Simplex s) => Semigroup (Chain s a) where
   Chain s0 <> Chain s1 = Chain $ s0 `simplexAppend` s1

instance (Ord a, Simplex s) => Monoid (Chain s a) where
   mempty   = Chain []

instance (Ord a, Simplex s) => Group (Chain s a) where
   invert = Chain . (inverse <$>) . getChain

instance (Ord a, Simplex s) => Abelian (Chain s a)

subsetList a b = foldr f True a where
   f = ((&&) . flip elem b)

instance (Eq a, Show a) => Eq (ListSimplex a) where
   (ListSimplex i a) == (ListSimplex i0 a0) =
      i == i0 && length a == length a0 && a `subsetList` a0

instance (Eq a, Show a) => PO.PartialOrd (ListSimplex a) where
  compare (ListSimplex i a) (ListSimplex i' b)
      | i        /= i'                         = Nothing
      | length a == length b && subsetList a b = Just EQ
      | length a <  length b && subsetList a b = Just LT
      | length a >  length b && subsetList b a = Just GT
      | otherwise                              = Nothing

  a <= b = PO.compare a b == Just LT || PO.compare a b == Just EQ

instance Simplex ListSimplex where
  emptySimplex                      = ListSimplex False []
  simplexAppend s0 s1 = foldr sAp [] $ sortBy simplexSort ((sortD <$>) $ s0 ++ s1) where
     simplexSort (ListSimplex _ l) (ListSimplex _ l0)
         | length l /= length l0 = compare (length l) (length l0)
         | otherwise                   = compare l l0 where
     sortD (ListSimplex i s) = ListSimplex i (sort s)
     sAp s [] = [s]
     sAp ls0@(ListSimplex i0 s0) ls1@(ListSimplex i1 s1 : ss)
        | s0 == s1 && i0 /= i1 = ss
        | otherwise            = ls0:ls1
  dimension     (ListSimplex i l) = length l - 1
  simplexToList (ListSimplex i l) = l
  simplexFromList = ListSimplex False
  isInverse (ListSimplex i _) = i
  --inverse   (ListSimplex i s) | trace "inverse" False = undefined 
  inverse   (ListSimplex i s) = ListSimplex (not i) s
  boundary  (ListSimplex i s) = zipWith ListSimplex (iterate not i) (filter (\l -> length l > 0) (boundary' s)) where
      --boundary' s | trace ("get Boundary: " ++ show s) False = undefined
      boundary' []     = []
      boundary' (s:ss) = ss : (map (s:) $ boundary' ss)

instance Simplex s => Simplex (DSimplex s) where
  emptySimplex                 = DSimplex emptySimplex 0
  dimension     (DSimplex l d) = dimension     l
  simplexToList (DSimplex l d) = simplexToList l
  simplexFromList l            = DSimplex (simplexFromList l) 0
  inverse       (DSimplex l d) | trace "bad inverse" False = undefined
  inverse       (DSimplex l d) = DSimplex (inverse l) d
  isInverse     (DSimplex l d) | trace "bad is inverse" False = undefined
  isInverse     (DSimplex l d) = isInverse l
  boundary      (DSimplex l d) = flip DSimplex d <$> (boundary l) 

instance {-#OVERLAPS#-} (Ord a, FSimplex s) => Ord (s a) where
  compare a b | False = undefined
              | degree    a /= degree    b = compare (degree    a) (degree    b)
              | dimension a /= dimension b = compare (dimension b) (dimension a)
              | otherwise                  = compare (sort . simplexToList $ a) (sort . simplexToList $ b)

instance {-#OVERLAPS#-} (Eq a, Simplex s) => Eq (s a) where
  l == l0 | dimension l /= dimension l0 = False
          | otherwise                   = all (uncurry (==) ) $ zip (simplexToList l) (simplexToList l0)

--instance {-#OVERLAPPABLE#-} (Ord a, Simplex s) => Ord (s a) where
--  compare l l0 | dimension l /= dimension l0 = compare (dimension l) (dimension l0)
--               | otherwise                   = compare (sortSimplex l) (sortSimplex l0) where
--             sortSimplex = sort . simplexToList
--
instance PIntervals ListsPIntervals where
    addInterval (ListsPIntervals intervals) dim pInterval = ListsPIntervals $
                         add <$> zip [0..] intervals' where
        add (i, iHomology) | i == dim = pInterval : iHomology
                           | True     =             iHomology
        fitMdHomology intervals
            | length intervals > dim = intervals
            | otherwise              = intervals ++ (take dimensionsDif $ repeat [])
        dimensionsDif = (dim - length intervals + 1)
        intervals' = fitMdHomology intervals

    listKIntervals (ListsPIntervals p) k | k < length p = p !! k
                                         | otherwise    = []
    listIntervals  = getListsPIntervals
    emptyPIntervals = ListsPIntervals []

instance Functor ListFiltration where
   fmap f (ListFiltration lf) =ListFiltration $ fmap f lf

instance Foldable ListFiltration where
   foldMap fm (ListFiltration f) = foldMap fm f

instance Filtration ListFiltration where
   emptyFiltration = ListFiltration []
   addSimplex (ListFiltration l) s = ListFiltration (s:l)
   updDegrees f (ListFiltration l) = ListFiltration $ (updDegree f) <$> l
   toListSimplices (ListFiltration l) = l
   fromListSimplices s = ListFiltration (sort s)

instance Simplex a => FSimplex (DSimplex a) where
  fsimplexFromList l d = DSimplex (simplexFromList l) d
  degree = degreeSimplex
  updDegree f s = s {degreeSimplex = f $ degreeSimplex s}

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
