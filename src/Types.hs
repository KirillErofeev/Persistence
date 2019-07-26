{-#language KindSignatures #-}
module Types where

import qualified Data.PartialOrd as PO

class Simplex s where
  dimension       :: s a -> Int
  simplexToList   :: s a -> [a]
  simplexFromList :: [a] -> s a
  -- length . simplexToList $ s a = 1 + (dimension $ s a)
  --
class Simplex s => FSimplex s where
    fsimplex  :: [a] -> Int -> s a
    degree    :: s a -> Int
    updDegree :: (Int -> Int) -> s a -> s a
    incDegree :: s a -> s a
    incDegree = updDegree (+1)

class Filtration f where
   emptyFiltration :: f s a
   addSimplex :: (FSimplex s) => f s a -> s a -> f s a
   updDegrees :: (FSimplex s) => (Int -> Int) -> f s a -> f s a
   --toListSimpleces :: f s a -> [s a]

   incDegrees :: (FSimplex s) => f s a -> f s a
   incDegrees = updDegrees (+1)

data ListSimplex a = ListSimplex [a]
data DSimplex simplex a = DSimplex {simplex :: simplex a, dSimplex :: Int}
data Point a = Point {x :: a, y :: a} deriving (Eq, Ord)
data PointCloud a = PointCloud [Point a]
data ListFiltration (fsimplex :: * -> *) a = ListFiltration [fsimplex a]
