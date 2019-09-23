{-#language KindSignatures #-}
{-#language MultiParamTypeClasses #-}
module Types where

import qualified Data.PartialOrd as PO

class Simplex s where
  inverse         :: s a -> s a
  boundary        :: s a -> [s a]
  isInverse       :: s a -> Bool
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
   addSimplex      :: (FSimplex s) => f s a -> s a -> f s a
   updDegrees      :: (FSimplex s) => (Int -> Int) -> f s a -> f s a
   toListSimplices :: f s a -> [s a]
   incDegrees :: (FSimplex s) => f s a -> f s a
   incDegrees = updDegrees (+1)

class PIntervals p where
    emptyPIntervals :: p a
    addInterval     :: p a -> Int ->   PInterval a -> p a
    listKIntervals  :: p a -> Int ->  [PInterval a]
    listIntervals   :: p a ->        [[PInterval a]]

data ListSimplex a                         = ListSimplex {isInverseSimplex :: Bool, getListSimplex :: [a]} 
data DSimplex simplex a                    = DSimplex {simplex :: simplex a, dSimplex :: Int}
data Point a                               = Point {x :: a, y :: a} deriving (Eq, Ord)
data PointCloud a                          = PointCloud [Point a]
data ListFiltration (fsimplex :: * -> *) a = ListFiltration [fsimplex a]
data PInterval a                           = PInterval {pStart :: a, pFinish :: a} 
data ListsPIntervals a                     = ListsPIntervals {getListsPIntervals :: [[PInterval a]]}

