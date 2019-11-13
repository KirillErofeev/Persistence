module Filtration where

import Data.List (foldl', sort)
import qualified Data.Set as Set
import Data.Monoid
import Data.Semigroup

import Types
import Instances

instance Bounded Double where
    maxBound = 2^1023
    minBound = -2^1023

buildFiltration distance points maxDim maxDist = concat $ snd $
    foldl' f (zeroDimSimplicies, [zeroDimSimplicies]) [1..maxDim-1] where
        zeroDimSimplicies = --foldl' (<>) mempty $
            pure DSimplex <*> ((expand emptySimplex) <$> points) <*> pure 0.0
        f (curDimSimplicies, simplicies) dim =
            let newSimplicies = addNewDim (dSimplex <$> curDimSimplicies)
            in (newSimplicies, newSimplicies:simplicies)
        addNewDim baseSimplicies = replaysFilter $ concat $ findNewSimplicies <$> baseSimplicies
        findNewSimplicies simplex = addSimplex simplex <$> 
            filter (not . flip elem simplex) points
        addSimplex simplex point = DSimplex (expand simplex point) $
            getMax (foldMap (Max . (distance point)) simplex)

replaysFilter ss = (Set.toList . Set.fromList) $ sortSimplex <$> ss where
    sortSimplex (DSimplex (ListSimplex i s) d) = DSimplex (ListSimplex i (sort s)) d

testF = goodShow $ (buildFiltration ((abs .) . (-)) [1,2,7] 5 7 :: [DSimplex ListSimplex Double])

goodShow s = putStr $ foldMap ((++ "\n") . goodShow') s
goodShow' (DSimplex (ListSimplex _ s) d) = show s ++ "->" ++ show d

points = [1,2,3]
