--{-#language MultiParamTypeClasses #-}
{-#language RankNTypes #-}
{-#language FlexibleInstances #-}
{-#language FlexibleContexts #-}
{-#language NoMonomorphismRestriction #-}
{-#language NamedFieldPuns#-}
{-#language IncoherentInstances#-}

module Homology where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe (fromJust, isJust)
import Data.Group

import qualified Numeric.Additive.Class as Additive ((+))
import qualified Numeric.Additive.Group as Group (negate)
import qualified Numeric.Algebra.Division as Division (recip)
import Numeric.Module.Class ((.*))

import Control.Applicative (ZipList(..))

import Types
import Instances

import Numeric.Field.Fraction
import Numeric.Field.Class

--import Debug.Trace
trace = flip const


-- https://geometry.stanford.edu/pape_rs/z-cph-05/zc-cph-05.pdf
-- should use some map tree instead of list of T_

---computePersistentHomology one filtration = undefined
computePersistentHomology one filtration = trace (show . sort $ filtration) computePersistentHomologyOnSorted one $ 
                                sort filtration
computePersistentHomologyOnSorted one filtration = snd $ let
   (tArray', pIntervals') = 
      foldl' (computeFiniteIntervals one) (tArray filtration, pIntervals ) (dSimplex <$> filtration) 
          in
      foldl' computeInfiniteIntervals     (tArray'          , pIntervals') (dSimplex <$> filtration) 

computeInfiniteIntervals (tAr, pIn) simplex
   | isMarked tAr simplex && isEmpty tAr simplex = (tAr,
      addInterval pIn (dimension simplex) (PIntervalInfinite $ findDegree tAr simplex))
   | otherwise = (tAr, pIn)

computeFiniteIntervals one (tAr, pIn) simplex = trace ("SIMPLEX: " ++ show simplex ) $ case removePivotRows one tAr simplex of
   FChain [] -> trace ("-----------------" ++ show simplex ++ " empty ------------------------\n") $
      (updateSimplex tAr (\s -> s {tElemIsMarked_ = True}) simplex, pIn         )
   d         -> trace ("Not empty:" ++ show d ++ "------------------------\n")  $
      (addBoundary 0 , addPInterval) where
      addBoundary i  = updateSimplex tAr (\s -> s {tElemBoundary_ = Just d}) (dSimplex maxIndSimplex)
      addPInterval  = addInterval pIn (dimension maxIndSimplex)
         (PIntervalFinite (degreeSimplex maxIndSimplex) (findDegree tAr simplex))
      maxIndSimplex = maxIndex tAr d

tArray filtration = getZipList $
       (ZipList $ (T_ . dSimplex) <$> listF)     <*> ZipList (repeat False) <*>
       (ZipList $ degreeSimplex <$> listF) <*> ZipList (repeat Nothing) where
    listF = foldr (:) [] filtration
pIntervals = emptyPIntervals :: ListsPIntervals a

removePivotRows one tAr simplex = snd $ head $ dropWhile fst $
    iterate (removePivotRows' one tAr) $
    (True, (removeMarkedTerms $ boundary one simplex)) where
        removeMarkedTerms = FChain . (filter (isMarked tAr . snd)) . getFChain  
removePivotRows' one tAr (p, FChain []) = trace ("BOUNDARY == 0")(False, FChain [])
removePivotRows' one tAr (p,ss) =
   case (tElemBoundary_ . findSimplex tAr . dSimplex . maxIndex tAr $ ss) of
      Nothing -> trace ("T[i] is empty == 0") (False, ss)
      Just b  -> trace ("D /= 0: b:" ++ show b ++ " ss: " ++ show ss ++ " q: " ++ show q ++ " Division.recip q: " ++ show (Division.recip q) ) 
                (True, ss Additive.+ Group.negate (Division.recip q .* b)) where
         q = coeffElemInChain b (tElemSimplex_ . findSimplex tAr . dSimplex . maxIndex tAr $ ss)
         findCoeff s s' | s == s' = True
                        | otherwise                    = False
         maxSm = (dSimplex .) . maxIndex

coeffElemInChain (FChain chain) element = fst . head . dropWhile ((/=element) . snd) $ chain 

makeFChain :: (Field f) => f -> [s a] -> FChain f s a
makeFChain one ss = FChain $ (\x -> (one, x)) <$> ss

simpliciesFromFChain (FChain chain) = snd <$> chain

makeFChain0 :: (Field f) => f -> [s a] -> [(f, s a)]
makeFChain0 one ss = (\x -> (one, x)) <$> ss

isEmpty tAr  = not . isJust . tElemBoundary_ . findSimplex tAr
isMarked tAr = tElemIsMarked_ . findSimplex tAr

maxIndex tAr (FChain boundary) = maximum $
    zipWith DSimplex (snd <$> boundary) $ findDegree tAr <$> (snd <$> boundary)
findDegree tAr = tElemDegree_ . findSimplex tAr
findSimplex tAr simplex = fromJust $
       find (\s -> tElemSimplex_ s == simplex) tAr

--updateSimplex tAr f simplex = foldr updateSimplex' [] tAr where
--    updateSimplex' t ts | t == t_ simplex = f t : ts
--                        | otherwise       =   t : ts

updateSimplex tAr f simplex = foldr updateSimplex' [] tAr where
    updateSimplex' t ts | tElemSimplex_ t == simplex = f t : ts
                        | otherwise       =   t : ts



