import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding ((+), (*), negate)
import Numeric.Field.Fraction
import qualified Numeric.Algebra.Division as Division (recip)
import Numeric.Module.Class
import Numeric.Additive.Class
import Numeric.Additive.Group
import Numeric.Algebra
import FiniteFields

import Types
import Instances
import Homology


main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [finiteFieldsTests, rationalFieldTests, chainComplexTests, homologyTests]

finiteFieldsTests = testGroup "Finite Fields tests"
    [
     testCase "Z2 is Additive" $ Z2Zero + Z2Unit @?= Z2Unit
    ,testCase "Z2 is Additive" $ Z2Unit + Z2Unit @?= Z2Zero
    ,testCase "Z2 is Additive" $ Z2Zero + Z2Zero @?= Z2Zero
    ,testCase "Z2 is Additive" $ z2GroupTest     @?= Z2Zero
    ]

z2GroupTest = Z2Unit + Z2Zero + negate Z2Unit * Z2Unit + Z2Zero * (negate Z2Unit) + negate Z2Zero

rationalFieldTests = testGroup "Rational Field tests"
    [
     testCase "Division invert trivial"   $ divisionInvertTest     @?= divisionInvertAnswer
    ,testCase "Division invert"           $ divisionInvertTest0    @?= divisionInvert0Answer
    ]

divisionInvertTest   = [Division.recip (1%1), Division.recip ((-1)%1)]
divisionInvertAnswer :: [Fraction Integer]
divisionInvertAnswer = [(1%1), (-1)%1]

divisionInvertTest0   = [Division.recip (1%7), Division.recip ((-13)%3)]
divisionInvert0Answer :: [Fraction Integer]
divisionInvert0Answer = [(7%1), (-3)%13]

chainComplexTests = testGroup "Chain Complex tests"
    [
     testCase "FChain additive semigroup" $ fChainAddTest          @?= fChainAddTestAnswer
    ,testCase "Boundary 1-simplex"        $ boundaryTest           @?= boundaryTestAnswer
    ,testCase "Boundary 0-simplex"        $ zeroBoundaryTest       @?= zeroBoundaryTestAnswer
    ,testCase "Boundary 2-simplex"        $ twoSimplexBoundaryTest @?= twoSimplexBoundaryAnswer
    ,testCase leftModuleTestName          $ leftModuleTest         @?= leftModuleAnswer
    ]

fChainExample :: FChain (Fraction Integer) ListSimplex Int
fChainExample = fChain 3 [0] <> fChain (-16) [1] <> fChain 7 [3] <> fChain 11 [1] <> fChain' 10 3 [0]

leftModuleTestName = "LeftModule FChain over Integer (+)"
leftModuleTest = (3 :: Integer) .* fChainExample
leftModuleAnswer = fChain 21 [3] <> fChain (-15) [1] <> fChain 19 [0]

fChain :: Integer -> [a] -> FChain (Fraction Integer) ListSimplex a
fChain c s = FChain $ [(c%1, ListSimplex False s)]
fChain' c c0 s = FChain $ [(c%c0, ListSimplex False s)]
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

-- https://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf
homologyTests = testGroup "Compute homology tests"
    [
     testCase "Carlsson test over fractions field" $ carlssonTest     @?= carlssonTestAnswer
    ,testCase "Carlsson test over Z2 "             $ carlssonZ2Test   @?= carlssonZ2Answer
    ]

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

carlssonZ2Test = computePersistentHomology Z2Unit filtration where
   --filtration :: ListFiltration (DSimplex ListSimplex Int)
   filtration = ListFiltration $ uncurry s <$>
                [([0]    , 0), ([1],   0),
                 ([2]    , 1), ([3],   1), ([0,1], 1), ([1,2], 1),
                 ([2,3]  , 2), ([0,3], 2),
                 ([0,2]  , 3),
                 ([0,1,2], 4),
                 ([0,2,3], 5)]
   s ls d = DSimplex (ListSimplex False ls) d

carlssonZ2Answer = ListsPIntervals $
    [[PIntervalInfinite 0, PIntervalFinite 1 2, PIntervalFinite 1 1, PIntervalFinite 0 1]
    ,[PIntervalFinite 2 5, PIntervalFinite 3 4]]
