import Test.Tasty
import Test.Tasty.HUnit

import Types
--import Instances
import Homology

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [testCase "Carlsson test" $ carlssonTest @?= carlssonTestAnswer]

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

carlssonTestAnswer = ListsPIntervals $
    [[PIntervalInfinite 0, PIntervalFinite 1 2, PIntervalFinite 1 1, PIntervalFinite 0 1]
    ,[PIntervalFinite 2 5, PIntervalFinite 3 4]]


