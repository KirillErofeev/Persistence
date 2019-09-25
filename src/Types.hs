{-#language KindSignatures #-}
{-#language MultiParamTypeClasses #-}
module Types where

import qualified Data.PartialOrd as PO
import qualified Data.Sequence   as Seq

class Simplex s where
  emptySimplex    ::  s a
  simplexAppend   :: [s a] -> [s a] -> [s a]
  inverse         ::  s a  -> s a
  boundary        ::  s a  -> [s a]
  isInverse       ::  s a  -> Bool
  dimension       ::  s a  -> Int
  simplexToList   ::  s a  -> [a]
  simplexFromList ::   [a] -> s a
  -- length . simplexToList $ s a = 1 + (dimension $ s a)

class Simplex s => FSimplex s where
    fsimplex  :: [a] -> Int -> s a
    degree    :: s a -> Int
    updDegree :: (Int -> Int) -> s a -> s a
    incDegree :: s a -> s a
    incDegree = updDegree (+1)

class (Foldable f) => Filtration f where
   emptyFiltration :: f s
   addSimplex      :: (FSimplex s) => f (s a) -> s a -> f (s a)
   updDegrees      :: (FSimplex s) => (Int -> Int) -> f (s a) -> f (s a)
   toListSimplices :: f s -> [s]
   incDegrees :: (FSimplex s) => f (s a) -> f (s a)
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
data ListFiltration simplex                = ListFiltration [simplex]
data ListsPIntervals a                     = ListsPIntervals {getListsPIntervals :: [[PInterval a]]}
data PInterval a                           = PIntervalFinite   {pStart :: a, pFinish :: a} |
                                             PIntervalInfinite {pStart :: a}

-- https://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf
data T_ s = T_ {tElemSimplex_ :: s  , tElemIsMarked_ :: Bool     , 
                tElemDegree_  :: Int, tElemBoundary_ :: Maybe [s]}
--data T_ s = TEmpty_ {tElemSimplex_ :: s, tElemIsMarked_ :: Bool, tElemDegree :: Int} | 
--    TElem_ {tElemSimplex_ :: s, tElemIsMarked_ :: Bool, tElemBoundary_ :: Maybe [s], tElemDegree_ :: }
--       deriving (Show)

