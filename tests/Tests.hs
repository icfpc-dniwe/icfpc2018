{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Data.Vector as V
import Linear.V3 (V3(..))
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.ChasingBottoms
import Data.Either (isLeft, isRight)
import Linear.Vector ((*^))

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Tensor3 (Tensor3, I3)
import ICFPC2018.Scoring
import ICFPC2018.Simulation
import qualified ICFPC2018.Tensor3 as T3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    tensor3Tests
  , simulationTests
  ]

tensor3Tests :: TestTree
tensor3Tests = testGroup "Tensor3 Tests" [tensor3Flip, tensor3InvalidIndex, tensor3InvalidUpdate, testScoring]

instance Arbitrary a => Arbitrary (Tensor3 a) where
  arbitrary = do
    size <- V3 <$> getSize <*> getSize <*> getSize
    values <- vectorOf (product size) arbitrary
    return $ T3.create (V.fromList values) size

genI3 :: Tensor3 a -> Gen I3
genI3 tensor = mapM (\sz -> choose (0, sz - 1)) (T3.size tensor)

tensor3Flip :: TestTree
tensor3Flip = QC.testProperty "Flip Tensor3 value" $ do
  tensor <- arbitrary `suchThat` ((/= 0) . product . T3.size)
  i <- genI3 tensor
  let value = tensor T3.! i
      tensor' = T3.update tensor [(i, not value)]
      value' = tensor' T3.! i
  return $ value == not value'

tensor3InvalidIndex :: TestTree
tensor3InvalidIndex = QC.testProperty "Invalid Tensor3 index" $ \(tensor :: Tensor3 ()) -> isBottom $ tensor T3.! T3.size tensor

tensor3InvalidUpdate :: TestTree
tensor3InvalidUpdate = QC.testProperty "Invalid Tensor3 update" $ \(tensor :: Tensor3 ()) -> isBottom $ T3.update tensor [(T3.size tensor, ())]

testScoring :: TestTree
testScoring = QC.testProperty "Scoring for commands" $ all id cmdTests where
  model = T3.create (V.fromList [
    False, False, False,
    False, False, False,
    False, False, False,

    False, True, True,
    False, True, True,
    False, False, False,

    False, True, True,
    False, True, True,
    False, False, False
    ]) (V3 3 3 3)
  cmdTests = [
    scoreCommand model Halt == 0,
    scoreCommand model Wait == 0,
    scoreCommand model Flip == 0,
    scoreCommand model (SMove (V3 0 2 1)) == 2 * 3,
    scoreCommand model (LMove (V3 0 2 1) (V3 1 0 0)) == 2 * (2 + 1 + 2),
    scoreCommand model (Fission (V3 1 1 1) 1) == 24,
    scoreCommand model (FusionP (V3 1 1 1)) == -24,
    scoreCommand model (FusionS (V3 1 1 1)) == 0,
    scoreCommand model (Fill (V3 2 1 1)) == 12 {-6-},
    scoreCommand model (Fill (V3 0 0 0)) == 12,
    scoreTrace model (V.fromList <$> [
      [Wait, LMove (V3 0 2 1) (V3 1 0 0)],
      [Flip, Wait],
      [SMove (V3 0 2 1), Flip],
      [Fill (V3 2 1 1), Fill (V3 0 0 0)],
      [Halt, Halt]
      ]) == sum [sum [0, 0, 6, 12 {-6-}, 0], sum [10, 0, 0, 12, 0]] + 20 * 2 * 5 + (3 + 3 + 30 + 3 + 3) * 27
    ]


simulationTests :: TestTree
simulationTests = testGroup "Simulation Tests" [
    simulationSMove
  , simulationSMoveOutOfBounds
  , simulationSMoveNonVoid
  ]

newtype EmptySingleBotModel = EmptySingleBotModel SingleBotModel deriving Show
instance Arbitrary EmptySingleBotModel where
  arbitrary = (max 1 <$> getSize) >>= \s -> return $ EmptySingleBotModel (startModel (V3 s s s))

newtype VolatileCoordinateWrapper = VolatileCoordinateWrapper VolatileCoordinate deriving Show
instance Arbitrary VolatileCoordinateWrapper where
  arbitrary = do
    [x, y, z] <- getSize >>= \s -> sequence . replicate 3 $ choose (0, s)
    return $ VolatileCoordinateWrapper (V3 x y z)

instance Arbitrary Axis where
  arbitrary = arbitraryBoundedEnum

newtype LongDifferenceWrapper = LongDifferenceWrapper LongDifference deriving Show
instance Arbitrary LongDifferenceWrapper where
  arbitrary = LongDifferenceWrapper <$> (mkLinearDifference <$> arbitrary <*> (choose (1, maxLLD)))



isBounded :: SingleBotModel -> VolatileCoordinate -> Bool
isBounded m r = let
  (V3 mx my mz) = T3.size $ filledModel m
  (V3 rx ry rz) = r
  in (0 < rx && rx < mx) && (0 < ry && ry < my) && (0 < rz && rz < mz)

simulationSMove :: TestTree
simulationSMove = QC.testProperty "SMove" $ \(
    EmptySingleBotModel m
  , VolatileCoordinateWrapper c
  , LongDifferenceWrapper d
  ) -> isBounded m (c+d) ==> isRight $ simulateStep m {botPos = c} (SMove d)

simulationSMoveOutOfBounds :: TestTree
simulationSMoveOutOfBounds = QC.testProperty "SMove: out of bounds" $ \(
    EmptySingleBotModel m
  , VolatileCoordinateWrapper c
  , axis
  , dir
  ) -> let
    l = maximum (T3.size $ filledModel m)
    d = mkLinearDifference axis ((if dir then 1 else (-1)) * l)
    in isLeft $ simulateStep m {botPos = c} (SMove d)

simulationSMoveNonVoid :: TestTree
simulationSMoveNonVoid = QC.testProperty "SMove: non void" $ \(
    EmptySingleBotModel m
  , VolatileCoordinateWrapper c
  , LongDifferenceWrapper d
  , NonEmpty ps
  ) -> let
    l = mlen d + 1
    n = normalizeLinearDifference d
    ps' = map (`mod` l) ps
    updates = zip (map (\i -> c + i*^n) ps') (repeat True)
    m' = m {
        botPos = c
      , filledModel = T3.update (filledModel m) updates
      }
    in isBounded m (c+d) ==> isLeft $ simulateStep m' (SMove d)
