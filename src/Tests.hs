{-#language FlexibleInstances #-}
{-#language NoMonomorphismRestriction #-}
module Tests where 

import qualified Data.PartialOrd as PO
import Data.List (permutations)
import Data.Group

import Types
--import Types0

import Homology

import Debug.Trace


--bnd [] = []
--bnd (x:xs) = xs : (map (x:) $ bnd xs)
----pi = 
----listOfListsPIntervalsTest0
----
--
--exampleFiltration = ListFiltration $
--   zipWith DSimplex  
--      (ListSimplex False <$> 
--         [[0],[1],[2],[3],[0,1],[1,2],[2,3],[0,3],[0,2],[0,1,2],[0,2,3]]) 
--         [0,0,1,1,1,1,2,2,3,4,5]
--
--exampleFiltration0 = 
--  ListFiltration $ (!! 4723430) p where
--   p = permutations . toListSimplices $ exampleFiltration

-- Test simplex op for Chain
--a = ListSimplex False [0]
--b = ListSimplex False [1]
--c = ListSimplex False [1,2]
--d = ListSimplex False [2,1]
--testChain = Chain [a, c, d, inverse c, inverse c, b, b, a, b, inverse a] <> Chain [c, c, c, inverse d]

-- Test simplex op for New Simplex 
--a = ListSimplex $ [ListSimplex_ False [0]  ] 
--b = ListSimplex $ [ListSimplex_ False [1]  ]
--c = ListSimplex $ [ListSimplex_ False [1,2]]
--d = ListSimplex $ [ListSimplex_ False [2,1]]

--testSimplexOp :: ListSimplex Int
--testSimplexOp = mempty<>a<>invert c<>d<>invert b<>invert c<>invert c<>invert b<>b<>a<>b<>invert a<>c<>c<>c<>invert d

--testInterval :: ListsPIntervals Int
--testInterval = addInterval (addInterval emptyPIntervals 3 (PIntervalFinite 7 223)) 5 (PIntervalFinite 7 9)
--sIn (ListsPIntervals l) = l
--
----primeStream :: (Integral a) => [a]
--primeStream :: [Int]
--primeStream = 2:3:5:7: (filter isPrime [11..])
----isPrime :: (Integral a) => a -> Bool
--isPrime :: Int -> Bool
--isPrime x = foldr (flip ((\f y -> f (x `mod` y /= 0)) . (&&))) True $ 
--   takeWhile (((. fromIntegral) . (>=) . sqrt . fromIntegral) x) primeStream 
--
----f n = foldr f' (0,0,0,0,0) (take n primeStream :: [Int]) where
----   f' x (x2,x1,x3,x7,x9) | x `mod` 10 == 2 = (x2+1,x1  ,x3,  x7,  x9  )
----                         | x `mod` 10 == 1 = (x2  ,x1+1,x3,  x7,  x9  )
----                         | x `mod` 10 == 3 = (x2  ,x1  ,x3+1,x7,  x9  )
----                         | x `mod` 10 == 7 = (x2  ,x1  ,x3,  x7+1,x9  )
----                         | x `mod` 10 == 9 = (x2  ,x1  ,x3,  x7,  x9+1)
--                         -- | otherwise       = trace ("asdf") (x2  ,x1  ,x3,  x7,  x9+1)
--                         --
--f n = foldr f' (0,0,0) (take n primeStream :: [Int]) where
--   f' x (x0,x1,x2) | x `mod` 3  == 0 = (x0+1,x1  ,x2  )
--                   | x `mod` 3  == 1 = (x0  ,x1+1,x2  )
--                   | x `mod` 3  == 2 = (x0  ,x1  ,x2+1)
--foldMap0 f l = foldr ((<>) . f) mempty l 
--
--foldr0 f e l = foldMap (f) id l 
