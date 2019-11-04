module Types0 where

import Data.Semigroup
import Data.Group
import Data.List (sortBy, sort)

class Simplex s where
  boundary            ::  s a  -> s a
  dimension           ::  s a  -> Int
  --isInverse           ::  s a  -> Bool
  --simplexToList       ::  s a  -> [a]
  --simplexFromList     ::   [a] -> s a

data ListSimplex_ a = ListSimplex_ {isInverse_ :: Bool, simplexToList_ :: [a]} deriving (Show)
data ListSimplex a  = ListSimplex [ListSimplex_ a] deriving (Show)

instance (Ord a) => Semigroup (ListSimplex a) where
   (ListSimplex s0) <> (ListSimplex s1) = ListSimplex $
      foldr sAp [] $ sortBy simplexSort ((sortD <$>) $ s0 ++ s1) where
         simplexSort (ListSimplex_ _ l) (ListSimplex_ _ l0)
            | length l /= length l0 = compare (length l) (length l0)
            | otherwise                   = compare l l0 where
         sortD (ListSimplex_ i s) = ListSimplex_ i (sort s)
         sAp s [] = [s]
         sAp ls0@(ListSimplex_ i0 s0) ls1@(ListSimplex_ i1 s1 : ss)
            | s0 == s1 && i0 /= i1 = ss
            | otherwise            = ls0:ls1
   
instance (Ord a) => Monoid (ListSimplex a) where
    mempty = ListSimplex []

instance (Ord a) => Group (ListSimplex a) where
    invert (ListSimplex s) = ListSimplex $ inv <$> s where
        inv (ListSimplex_ i s) = ListSimplex_ (not i) s 

instance (Ord a) => Simplex (ListSimplex a) where
    boundary  (ListSimplex s) = ListSimplex $ (boundary' <$> s) zipWith ListSimplex (iterate not i) (filter (\l -> length l > 0) (boundary' s)) where
    boundary  (ListSimplex i s) = zipWith ListSimplex (iterate not i) (filter (\l -> length l > 0) (boundary' s)) where
      boundary' []     = []
      boundary' (s:ss) = ss : (map (s:) $ boundary' ss)

data DSimplex simplex = DSimplex {simplexFromDSimplex :: simplex, simplexDegree :: Double}

class (Functor f, Foldable f) => Filtration f where
   emptyFiltration :: f s
   addSimplex      :: (FSimplex s) => f (s a) -> s a -> f (s a)
   updDegrees      :: (FSimplex s) => (Double -> Double) -> f (s a) -> f (s a)
   toListSimplices :: f s -> [s]
   fromListSimplices :: (FSimplex s, Ord (s a)) => [s a] -> f (s a)
   incDegrees :: (FSimplex s) => f (s a) -> f (s a)
   incDegrees = updDegrees (+1.0)

data ListFiltration simplex                = ListFiltration [simplex]

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
