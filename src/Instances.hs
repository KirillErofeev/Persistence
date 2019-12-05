{-#language FlexibleInstances #-}
{-#language FlexibleContexts #-}
{-#language MultiParamTypeClasses #-}
module Instances where

import Numeric.Field.Class

import qualified Data.PartialOrd as PO
import Data.List
import Data.Group
import Data.Pointed

import Numeric.Domain.PID
import Numeric.Field.Class
import Numeric.Module.Class
import qualified Numeric.Additive.Class as Additive
import qualified Numeric.Additive.Group as Group (negate, Group)
import qualified Numeric.Algebra.Class as Algebra ((*), Monoidal)
import qualified Numeric.Algebra.Division as Division (recip)
import qualified Numeric.Algebra as Natural (Natural)
import Numeric.Decidable.Zero (isZero)
import Numeric.Algebra.Class (zero)
import Numeric.Algebra.Unital (one)

import Types

import Debug.Trace

instance (Simplex s, Eq f, Eq (s a), Eq a) => Eq (T_ f s a) where
   t0 == t1 =
      tElemSimplex_ t0 == tElemSimplex_ t1
       || inverse (tElemSimplex_ t0) == tElemSimplex_ t1

instance (Eq f, Eq (s a)) => Eq (FChain f s a) where
    FChain f0 == FChain f1 = length f0 == length f1 &&
                             all (flip elem f1) f0

instance (Field f, Ord a, Simplex s) => Semigroup (FChain f s a) where
   --FChain s0 <> FChain s1 = FChain $ [((fst . head) s0 + (fst . head) s1, s0)]
   FChain s0 <> FChain s1 =  FChain $ filter (not . isZero . fst) $
       foldr reduce [] $ (sortBy simplexSort $ s0 <> s1) where
           simplexSort ss0 ss1 = compare (sort . simplexToList . snd $ ss0)
                                         (sort . simplexToList . snd $ ss1)
           reduce simplex [] = [simplex]
           reduce curSimplex@(f0, simplex0) checkedSimplicies@((f1, simplex1) : simplicies)
               | simplex0 == simplex1 = (f0 Additive.+ f1, simplex0) : simplicies
               | otherwise            = curSimplex : checkedSimplicies

instance (Field f, Ord a, Simplex s) => Monoid (FChain f s a) where
   mempty   = FChain []

instance (Field f, Ord a, Simplex s) => Group (FChain f s a) where
   invert = FChain . (inverse <$>) . getFChain where
       inverse (cf, s) = (Group.negate cf, s)

instance (Field f, Ord a, Simplex s) => Abelian (FChain f s a)

--- Kmett Instances:
instance (Field f, Ord a, Simplex s) => Additive.Additive (FChain f s a) where
    (+) = (<>)

plus a b = trace (show a ++ "plus" ++ show b) $ a <> b

instance (Field f, Ord a, Simplex s) => LeftModule f (FChain f s a) where
    f .* (FChain cs) = FChain $ filter (not . isZero . fst) $ (product f) <$> cs where
       product factor (f, s) = (factor Algebra.* f, s)

instance (Field f, Ord a, Simplex s) => LeftModule Integer (FChain f s a) where
    int .* chain   | int == 0 = FChain []
                   | int >  0 = last $ take (fromInteger int ) $ iterate (chain Additive.+ ) chain
                   | int <  0 = last $ take (-fromInteger int) $ iterate (invChain Additive.+) invChain where
        inv (f, s) = (Division.recip f, s)
        invChain  = FChain . (inv <$>) . getFChain $ chain

instance (Field f, Ord a, Simplex s) => RightModule Integer (FChain f s a) where
     chain *. int = int .* chain

instance (Field f, Ord a, Simplex s) => LeftModule Natural.Natural (FChain f s a) where
      nat .* chain = (toInteger nat) .* chain

instance (Field f, Ord a, Simplex s) => RightModule Natural.Natural (FChain f s a) where
     chain *. nat = nat .* chain

instance (Field f, Ord a, Simplex s) => Algebra.Monoidal (FChain f s a) where
    zero = FChain []

instance (Field f, Ord a, Simplex s) => Group.Group (FChain f s a) where
    negate (FChain c) = FChain $ inv <$> c where
        inv (f, s) = (Group.negate f, s)

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

instance Foldable ListSimplex where
    foldMap f (ListSimplex _ s) = foldMap f s

instance Simplex ListSimplex where
  emptySimplex                      = ListSimplex False []
  expand (ListSimplex i s) a = ListSimplex i (a:s)
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
  boundary  one (ListSimplex i s) = FChain $ 
    zip (zipWith ($) flipSigns (repeat one)) ((ListSimplex False <$>) . filter ((>0) . length) $ (boundary' s)) where
      --boundary' s | trace ("get Boundary: " ++ show s) False = undefined
      boundary' []     = []
      boundary' (s:ss) = ss : (map (s:) $ boundary' ss)
      flipSigns = id : Group.negate : flipSigns

instance (Simplex s) => Foldable (DSimplex s) where
    foldMap = undefined

-----instance Simplex s => Simplex (DSimplex s) where
-----  emptySimplex                 = DSimplex emptySimplex 0
-----  dimension     (DSimplex l d) = dimension     l
-----  simplexToList (DSimplex l d) = simplexToList l
-----  simplexFromList l            = DSimplex (simplexFromList l) 0
-----  inverse       (DSimplex l d) | trace "bad inverse" False = undefined
-----  inverse       (DSimplex l d) = DSimplex (inverse l) d
-----  isInverse     (DSimplex l d) | trace "bad is inverse" False = undefined
-----  isInverse     (DSimplex l d) = isInverse l
-----  boundary  one  = boundary one . dSimplex

--instance {-#OVERLAPS#-} (Ord a, FSimplex s) => Ord (s a) where
--  compare a b | False = undefined
--              | degree    a /= degree    b = compare (degree    a) (degree    b)
--              | dimension a /= dimension b = compare (dimension b) (dimension a)
--              | otherwise                  = compare (sort . simplexToList $ a) (sort . simplexToList $ b)

instance (Simplex s, Ord a) => Eq (DSimplex s a) where
    a@(DSimplex s0 d0) == b@(DSimplex s1 d1)
       | degreeSimplex a /= degreeSimplex b = False
       | dimension s0 /= dimension s1 = False
       | otherwise = (sort . simplexToList $ s0) == (sort . simplexToList $ s1)

instance (Simplex s, Ord a) => Ord (DSimplex s a) where
    compare a@(DSimplex s0 d0) b@(DSimplex s1 d1)
       | degreeSimplex a /= degreeSimplex b =
          compare (degreeSimplex a) (degreeSimplex b)
       | dimension s0 /= dimension s1 =
          compare (dimension s0) (dimension s1)
       | otherwise = compare (sort . simplexToList $ s0) (sort . simplexToList $ s1)

instance Simplex (DSimplex ListSimplex) where
  emptySimplex        = DSimplex (ListSimplex False []) 0
  simplexAppend d0 d1 = undefined
  dimension    (DSimplex s0 d0) = dimension s0
  boundary one (DSimplex s0 d0) = undefined

  --inverse             ::  s a  -> s a
  --isInverse           ::  s a  -> Bool
  --simplexToList       ::  s a  -> [a]
  --simplexFromList     ::   [a] -> s a
    

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

-----instance Simplex a => FSimplex (DSimplex a) where
-----  fsimplexFromList l d = DSimplex (simplexFromList l) d
-----  degree = degreeSimplex
-----  updDegree f s = s {degreeSimplex = f $ degreeSimplex s}

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
