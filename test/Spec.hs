import Test.Tasty
import Test.Tasty.HUnit

import Numeric.Field.Fraction

import Types
import Instances
import Homology


main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [testCase "Carlsson test" $ carlssonTest @?= carlssonTestAnswer
    ,testCase "FChain additive semigroup" $ fChainAddTest          @?= fChainAddTestAnswer
    ,testCase "Boundary test 1-simplex"   $ boundaryTest           @?= boundaryTestAnswer
    ,testCase "Boundary test 0-simplex"   $ zeroBoundaryTest       @?= zeroBoundaryTestAnswer
    ,testCase "Boundary test 2-simplex"   $ twoSimplexBoundaryTest @?= twoSimplexBoundaryAnswer
    ]

-- https://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf
carlssonTest = computePersistentHomology ((1%1) :: Fraction Integer) filtration where
   --filtration :: ListFiltration (DSimplex ListSimplex Int)
   filtration = ListFiltration $ uncurry s <$>
                [([0]    , 0), ([1],   0),
                 ([2]    , 1), ([3],   1), ([0,1], 1), ([1,2], 1),
                 ([2,3]  , 2), ([0,3], 2),
                 ([0,2]  , 3),
                 ([0,1,2], 4),
                 ([0,2,3], 5)]
   s ls d = DSimplex (ListSimplex False ls) d

carlssonTestAnswer = ListsPIntervals $
    [[PIntervalInfinite 0, PIntervalFinite 1 2, PIntervalFinite 1 1, PIntervalFinite 0 1]
    ,[PIntervalFinite 2 5, PIntervalFinite 3 4]]

fChain :: Integer -> [a] -> FChain (Fraction Integer) ListSimplex a
fChain c s = FChain $ [(c%1, ListSimplex False s)]
ls s = ListSimplex False s

fChainAddTest :: FChain (Fraction Integer) ListSimplex Int
fChainAddTest = fChain 3 [0] <> fChain (-4) [1] <> fChain 7 [3] <> fChain 11 [1] <> fChain 2 [0]

fChainAddTestAnswer :: FChain (Fraction Integer) ListSimplex Int
fChainAddTestAnswer = fChain 5 [0] <> fChain 7 [1] <> fChain 7 [3]

boundaryTest = boundary (1%1) (ls [0,1])

boundaryTestAnswer :: FChain (Fraction Integer) ListSimplex Int
boundaryTestAnswer = fChain (-1) [0] <> fChain 1 [1]

zeroBoundaryTest = boundary (1%1) (ls [0])
zeroBoundaryTestAnswer :: FChain (Fraction Integer) ListSimplex Int
zeroBoundaryTestAnswer = FChain []

twoSimplexBoundaryTest = boundary (1%1) (ls [3,7,11])
twoSimplexBoundaryAnswer :: FChain (Fraction Integer) ListSimplex Int 
twoSimplexBoundaryAnswer = fChain 1 [7,11] <> fChain (-1) [3,11] <> fChain 1 [3,7] 
