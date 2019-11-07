module Filtration where

import Data.List (foldl')
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
        addNewDim baseSimplicies = concat $ findNewSimplicies <$> baseSimplicies
        findNewSimplicies simplex = filter checkDist $ addSimplex simplex <$> points
        checkDist s = getAll $ foldMap (\f -> All (f $ degreeSimplex s)) [(>0), (<=maxDist)]
        addSimplex simplex point = DSimplex (expand simplex point) $
            getMin (foldMap (Min . (distance point)) simplex)

testF = goodShow $ (buildFiltration ((abs .) . (-)) [1,2,7] 5 7 :: [DSimplex ListSimplex Double])

goodShow s = putStr $ foldMap ((++ "\n") . goodShow') s
goodShow' (DSimplex (ListSimplex _ s) d) = show s ++ "->" ++ show d

points = [1,2,3]
