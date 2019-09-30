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

--metricFiltration' fDists pointCloud = foldl' addNewGrade emptyFiltration pds where
--commutativeApp f (ac, x:xs) = commutativeApp f ((f x <$> x:xs) ++ ac, xs)
--commutativeApp f (ac, []  ) = ac

--test :: ListFiltration ((DSimplex ListSimplex) (Point Double))
--test = metricFiltration' [0,1,3,100] [Point 0 0, Point 2 2, Point 1 1, Point 3 3] 


-- https://geometry.stanford.edu/pape_rs/z-cph-05/zc-cph-05.pdf
-- should use some map tree instead of list of T_
--computePersistentHomology :: (Filtration f, FSimplex s, PIntervals h) => f (s a) -> h b
--computePersistentHomology :: ListFiltration (DSimplex ListSimplex Int) -> ListsPIntervals Int
computePersistentHomology filtration = snd $ let
   (tArray', pIntervals') = foldl' computeFiniteIntervals (tArray, pIntervals) (dSimplex <$> filtration) 
                in foldl' computeInfiniteIntervals (tArray', pIntervals') (dSimplex <$> filtration) where
    computeInfiniteIntervals (tAr, pIn) simplex
       | isMarked tAr simplex && isEmpty tAr simplex =
          (tAr, addInterval pIn (dimension simplex) (PIntervalInfinite $ findDegree tAr simplex))
       | otherwise = (tAr, pIn)

    computeFiniteIntervals (tAr, pIn) simplex = trace (show pIn ++ "\nT: " ++ show tAr ++ "\nProcessed: " ++ show simplex) $ 
       case removePivotRows tAr simplex of
          [] -> (updateSimplex tAr (\s -> s {tElemIsMarked_ = True}) simplex, pIn)
          d  | trace ("ADDINTERVAL" ++ show (addInterval pIn (dimension (maxIndex tAr d))(PIntervalFinite (degree (maxIndex tAr d)) (findDegree tAr simplex)))) False -> undefined
          d  -> (addBoundary 0 , addPInterval) where
             addBoundary i  | trace ("Find: " ++ show (dSimplex maxIndSimplex)) False = undefined
             addBoundary i  = (updateSimplex tAr (\s -> s {tElemBoundary_ = Just d})) (dSimplex maxIndSimplex)
             addPInterval  = addInterval pIn (dimension maxIndSimplex)
                (PIntervalFinite (degree maxIndSimplex) (findDegree tAr simplex))
             maxIndSimplex = maxIndex tAr d
    --tArray :: [T_ (ListSimplex Int)]
    tArray = getZipList $
       (ZipList $ (T_ . dSimplex) <$> listF)     <*> ZipList (repeat False)   <*>
       (ZipList $ degree <$> listF) <*> ZipList (repeat Nothing)
    listF = foldr (:) [] filtration
    pIntervals = emptyPIntervals :: ListsPIntervals a
    --removePivotRows :: (Simplex s, Ord a,) => [T_ (s a)] -> s a -> [s a]
    --removePivotRows tAr simplex | trace ("RPR: " ++ show simplex ++ " Marked boundary: " ++ (show $  (removePivotRows' tAr) $  (True, filter (isMarked tAr) $ boundary simplex ))) False = undefined
    removePivotRows tAr simplex | trace ("RPR: " ++ show simplex ++ " Marked boundary: " ++ (show $ filter (isMarked tAr) $ boundary simplex )) False = undefined
    removePivotRows tAr simplex = snd $ head $ dropWhile fst $
       iterate (removePivotRows' tAr) $ (True, (filter (isMarked tAr) $ boundary simplex))
    --removePivotRows' :: [T_ (ListSimplex Int)] -> (Bool, [ListSimplex Int]) -> (Bool, [ListSimplex Int])
    removePivotRows' tAr (p,[]) | trace ("Chain: " ++ "[]") False = undefined
    removePivotRows' tAr (p,[]) = (False, [])
    --removePivotRows' tAr (p,ss) | trace ("RPR1': " ++ show (ss)) (ss == []) = undefined
    removePivotRows' tAr (p,ss) =
       case tElemBoundary_ . findSimplex tAr . dSimplex . maxIndex tAr $ ss of
          Nothing -> (False, ss            )
          Just b  | trace ("\nCHAIN: " ++ show ("SS: " ++ show ss ++ "\n B: " ++ show b  ++ "\nRESULT: " ++ show (getChain $ Chain ss <> invert (Chain b))) ++ "\n") False -> (True , getChain $ Chain ss <> invert (Chain b))
                  | simplexCoeff b (maxSm tAr ss) /=
                    simplexCoeff ss (maxSm tAr ss) ->
                     (True, getChain $ Chain ss <> Chain b)
                  | otherwise  ->  (True , getChain $ Chain ss <> invert (Chain b))
    simplexCoeff ss s = isInverse $ fromJust $ find findCoeff ss where
       findCoeff s' | s == s' || (inverse s) == s' = True
                    | otherwise                             = False
    maxSm = (dSimplex .) . maxIndex
    isEmpty tAr = not . isJust . tElemBoundary_ . findSimplex tAr
    isMarked tAr = tElemIsMarked_ . findSimplex tAr
    --maxIndex :: [T_ (ListSimplex Int)] -> [DSimplex ListSimplex Int] -> DSimplex ListSimplex Int
    --maxIndex tAr b | trace ("MAX INDEX: ") False = undefined
    maxIndex tAr boundary = maximum $ zipWith DSimplex boundary $ findDegree tAr <$> boundary
    --maxIndex tAr boundary = maximum boundary
    findDegree tAr = tElemDegree_ . findSimplex tAr
    findSimplex tAr simplex = --traceShow ("|||fs: " ++ show simplex) $
      fromJust $
       find (\s -> tElemSimplex_ s == reverseIfInverse simplex) tAr
    updateSimplex :: [T_ (ListSimplex Int)] -> (T_ (ListSimplex Int) -> T_ (ListSimplex Int)) -> ListSimplex Int -> [T_ (ListSimplex Int)]
    updateSimplex tAr f simplex = foldr updateSimplex' [] tAr where
       updateSimplex' t ts | t == t_ simplex = f t : ts
                           | otherwise       =   t : ts
    reverseIfInverse s | isInverse s = inverse s
                       | otherwise   = s


--updateSimplex :: (Simplex s, Eq a) => [T_ (s a)] -> (T_ (s a) -> T_ (s a)) -> s a -> [T_ (s a)]
--updateSimplex tAr f simplex = foldr updateSimplex' [] tAr where
--   updateSimplex' t ts | t == t_ simplex = f t : ts
--                       | otherwise       = t : ts
