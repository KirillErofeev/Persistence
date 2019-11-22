import Test.Tasty
import Test.Tasty.HUnit

import Numeric.Field.Fraction

import Types
import Instances
import Homology


--main :: IO ()
--main = defaultMain tests

--tests = testGroup "Tests" [unitTests]

--unitTests = testGroup "Unit tests"
--    [testCase "Carlsson test" $ carlssonTest @?= carlssonTestAnswer
--    ,testCase "FChain additive semigroup" $ fChainAddTest == fChainAddTestAnswer @?= True
--    ]
--
---- https://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf
--carlssonTest = computePersistentHomology filtration where
--   --filtration :: ListFiltration (DSimplex ListSimplex Int)
--   filtration = ListFiltration $ uncurry s <$>
--                [([0]    , 0), ([1],   0),
--                 ([2]    , 1), ([3],   1), ([0,1], 1), ([1,2], 1),
--                 ([2,3]  , 2), ([0,3], 2),
--                 ([0,2]  , 3),
--                 ([0,1,2], 4),
--                 ([0,2,3], 5)]
--   s ls d = DSimplex (ListSimplex False ls) d
--
--carlssonTestAnswer = ListsPIntervals $
--    [[PIntervalInfinite 0, PIntervalFinite 1 2, PIntervalFinite 1 1, PIntervalFinite 0 1]
--    ,[PIntervalFinite 2 5, PIntervalFinite 3 4]]
--
--fChain :: f -> [a] -> FChain f s a
--fChain c s = FChain $ [(c%1, ListSimplex False s)]
--ls s = ListSimplex False s
--
----fChainAddTest :: FChain Integer ListSimplex Int
--fChainAddTest = fChain 3 [0] <> fChain (-4) [1] <> fChain 7 [3] <> fChain 11 [1] <> fChain 2 [0]
--
----fChainAddTestAnswer :: FChain Integer ListSimplex Int
--fChainAddTestAnswer = FChain $ (ls <$>) <$> [(5,[0]), (7,[1]), (7,[3])]
--
--
