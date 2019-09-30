{-#language FlexibleInstances #-}
module Tests where 

import qualified Data.PartialOrd as PO
import Data.List (permutations)
import Data.Group

import Types

import Homology

a = ListSimplex False [0]
b = ListSimplex False [1]
c = ListSimplex False [1,2]
d = ListSimplex False [2,1]

bnd [] = []
bnd (x:xs) = xs : (map (x:) $ bnd xs)
--pi = 
--listOfListsPIntervalsTest0
--

exampleFiltration = ListFiltration $
   zipWith DSimplex  
      (ListSimplex False <$> 
         [[0],[1],[2],[3],[0,1],[1,2],[2,3],[0,3],[0,2],[0,1,2],[0,2,3]]) 
         [0,0,1,1,1,1,2,2,3,4,5]

exampleFiltration0 = 
  ListFiltration $ (!! 4723430) p where
   p = permutations . toListSimplices $ exampleFiltration

testChain = Chain [a, c, d, inverse c, inverse c, b, b, a, b, inverse a] <> Chain [c, c, c, inverse d]

-- https://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf
carlssonTest = computePersistentHomology filtration where
   filtration :: ListFiltration (DSimplex ListSimplex Int)
   filtration = ListFiltration $ uncurry s <$> 
                [([0]    , 0), ([1],   0), 
                 ([2]    , 1), ([3],   1), ([0,1], 1), ([1,2], 1),
                 ([2,3]  , 2), ([0,3], 2),
                 ([0,2]  , 3),
                 ([0,1,2], 4),
                 ([0,2,3], 5)]
   s ls d = DSimplex (ListSimplex False ls) d

testInterval :: ListsPIntervals Int
testInterval = addInterval (addInterval emptyPIntervals 3 (PIntervalFinite 7 223)) 5 (PIntervalFinite 7 9)
sIn (ListsPIntervals l) = l
