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

import Control.Applicative (ZipList(..))

import Types
import Instances

import Debug.Trace

-- https://geometry.stanford.edu/pape_rs/z-cph-05/zc-cph-05.pdf
-- should use some map tree instead of list of T_
--computePersistentHomology :: (Filtration f, FSimplex s, PIntervals h) => f (s a) -> h b
--computePersistentHomology :: ListFiltration (DSimplex ListSimplex Int) -> ListsPIntervals Int
computePersistentHomology filtration = snd $ let
   (tArray', pIntervals') = 
      foldl' computeFiniteIntervals   (tArray filtration, pIntervals ) (dSimplex <$> filtration) 
          in
      foldl' computeInfiniteIntervals (tArray'          , pIntervals') (dSimplex <$> filtration) 

computeInfiniteIntervals (tAr, pIn) simplex
   | isMarked tAr simplex && isEmpty tAr simplex = (tAr, 
      addInterval pIn (dimension simplex) (PIntervalInfinite $ findDegree tAr simplex))
   | otherwise = (tAr, pIn)

computeFiniteIntervals (tAr, pIn) simplex = case removePivotRows tAr simplex of
   [] -> (updateSimplex tAr (\s -> s {tElemIsMarked_ = True}) simplex, pIn         )
   d  -> (addBoundary 0                                              , addPInterval) where
      addBoundary i  = (updateSimplex tAr (\s -> s {tElemBoundary_ = Just d})) (dSimplex maxIndSimplex)
      addPInterval  = addInterval pIn (dimension maxIndSimplex)
         (PIntervalFinite (degree maxIndSimplex) (findDegree tAr simplex))
      maxIndSimplex = maxIndex tAr d

tArray filtration = getZipList $
       (ZipList $ (T_ . dSimplex) <$> listF)     <*> ZipList (repeat False) <*>
       (ZipList $ degree <$> listF) <*> ZipList (repeat Nothing) where
    listF = foldr (:) [] filtration
pIntervals = emptyPIntervals :: ListsPIntervals a

removePivotRows tAr simplex = snd $ head $ dropWhile fst $
       iterate (removePivotRows' tAr) $ (True, (filter (isMarked tAr) $ boundary simplex))

removePivotRows' tAr (p,[]) = (False, [])
removePivotRows' tAr (p,ss) =
   case (tElemBoundary_ . findSimplex tAr . dSimplex . maxIndex tAr $ ss) of
      Nothing -> (False, ss)
      Just b | simplexCoeff b (maxSm tAr ss) /= simplexCoeff ss (maxSm tAr ss) ->
               (True, getChain $ Chain ss <>         Chain b)
             | otherwise                                                       ->  
               (True, getChain $ Chain ss <> invert (Chain b)) where
         simplexCoeff ss s = isInverse $ fromJust $ find (findCoeff s) ss
         findCoeff s s' | s == s' || (inverse s) == s' = True
                        | otherwise                    = False
         maxSm = (dSimplex .) . maxIndex

isEmpty tAr  = not . isJust . tElemBoundary_ . findSimplex tAr
isMarked tAr = tElemIsMarked_ . findSimplex tAr
maxIndex tAr boundary = maximum $ zipWith DSimplex boundary $ findDegree tAr <$> boundary
findDegree tAr = tElemDegree_ . findSimplex tAr
findSimplex tAr simplex = fromJust $
       find (\s -> tElemSimplex_ s == reverseIfInverse simplex) tAr

--updateSimplex :: [T_ (ListSimplex Int)] -> (T_ (ListSimplex Int) -> T_ (ListSimplex Int)) -> ListSimplex Int -> [T_ (ListSimplex Int)]
updateSimplex tAr f simplex = foldr updateSimplex' [] tAr where
    updateSimplex' t ts | t == t_ simplex = f t : ts
                        | otherwise       =   t : ts
reverseIfInverse s | isInverse s = inverse s
                   | otherwise   = s


--updateSimplex :: (Simplex s, Eq a) => [T_ (s a)] -> (T_ (s a) -> T_ (s a)) -> s a -> [T_ (s a)]
--updateSimplex tAr f simplex = foldr updateSimplex' [] tAr where
--   updateSimplex' t ts | t == t_ simplex = f t : ts
--                       | otherwise       = t : ts
