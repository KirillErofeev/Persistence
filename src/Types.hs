{-#language KindSignatures #-}
{-#language MultiParamTypeClasses #-}
module Types where

import qualified Data.PartialOrd as PO
import qualified Data.Sequence   as Seq
import Data.List

class Simplex s where
  emptySimplex    ::  s a
  simplexAppend   :: Ord a => [s a] -> [s a] -> [s a]
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

class (Functor f, Foldable f) => Filtration f where
   emptyFiltration :: f s
   addSimplex      :: (FSimplex s) => f (s a) -> s a -> f (s a)
   updDegrees      :: (FSimplex s) => (Int -> Int) -> f (s a) -> f (s a)
   toListSimplices :: f s -> [s]
   fromListSimplices :: (FSimplex s, Ord (s a)) => [s a] -> f (s a)
   incDegrees :: (FSimplex s) => f (s a) -> f (s a)
   incDegrees = updDegrees (+1)

class PIntervals p where
    emptyPIntervals :: p a
    addInterval     :: p a -> Int ->   PInterval a -> p a
    listKIntervals  :: p a -> Int ->  [PInterval a]
    listIntervals   :: p a ->        [[PInterval a]]

class Metric a where
  distance :: a -> a -> Double

data ListSimplex a                         = ListSimplex {isInverseSimplex :: Bool, getListSimplex :: [a]} 
data DSimplex simplex a                    = DSimplex {dSimplex :: simplex a, degreeSimplex :: Int}
data Point a                               = Point {x :: a, y :: a} deriving (Eq, Ord)
data PointCloud a                          = PointCloud [Point a]
data ListFiltration simplex                = ListFiltration [simplex]
data ListsPIntervals a = ListsPIntervals {getListsPIntervals :: [[PInterval a]]} 
data PInterval a                           = PIntervalFinite   {pStart :: a, pFinish :: a} |
                                             PIntervalInfinite {pStart :: a} deriving Show

newtype Chain s a = Chain {getChain :: [s a]} deriving (Show)

-- https://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf
data T_ s = T_ {tElemSimplex_ :: s  , tElemIsMarked_ :: Bool     , 
                tElemDegree_  :: Int, tElemBoundary_ :: Maybe [s]}
                
t_ simplex = T_ simplex False 0 Nothing

instance (Show a) => Show (ListsPIntervals a) where
   show (ListsPIntervals l) = ("\nPINTERVALS:" ++) . emptyProcess $ 
      foldMap stMap (zip [0..] l) where
       stMap (k,ins) = "\nk = " ++ show k ++ " ::: " ++ foldMap showIntervals ins
       showIntervals (PIntervalFinite s f) = "(" ++ show s ++ "," ++ show f ++ ")"
       showIntervals (PIntervalInfinite s) = "(" ++ show s ++ ",inf)" 
       emptyProcess "" = "[]"
       emptyProcess s  = s

instance (Show s) => Show (T_ s) where
   show (T_ s b d boundary) = "|" ++ show s ++ dShow ++ bShow ++ boundShow ++ "|" where
      dShow = "^" ++ show d
      bShow = case b of
         False -> ""
         True  -> "*"
      boundShow = case boundary of
         Nothing    -> ""
         Just bound -> "<" ++ show boundary ++ ">"

