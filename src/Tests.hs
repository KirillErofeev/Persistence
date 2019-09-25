module Tests where 

import qualified Data.PartialOrd as PO
import Data.List (permutations)
import Data.Group

import Types

import Homology

a = ListSimplex True  [1,2,3] :: ListSimplex Int 
b = ListSimplex False [1,2]   :: ListSimplex Int
c = ListSimplex True  [1]   :: ListSimplex Int

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

instance Monoid [Simplex s] where
   mempty   = emptySimplex
   s0 <> s1 = 
