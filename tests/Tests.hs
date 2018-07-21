{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map as M
import Linear.V3 (V3(..))
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.ChasingBottoms

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Tensor3 (Tensor3, I3)
import ICFPC2018.Simulation
import ICFPC2018.Model
import qualified ICFPC2018.Tensor3 as T3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = adjustOption (min 16 :: QC.QuickCheckMaxSize -> QC.QuickCheckMaxSize) $ testGroup "Tests"
  [ tensor3Tests
  , simulationTests
  , aStarTests
  , packTests
  ]

--
-- Tensor3 Tests
--

tensor3Tests :: TestTree
tensor3Tests = testGroup "Tensor3 Tests" [tensor3Flip, tensor3InvalidIndex, tensor3InvalidUpdate]

instance Arbitrary a => Arbitrary (Tensor3 a) where
  arbitrary = do
    size <- V3 <$> getSize <*> getSize <*> getSize
    values <- vectorOf (product size) arbitrary
    return $ T3.create (V.fromList values) size

genI3 :: Tensor3 a -> Gen I3
genI3 tensor = mapM (\sz -> choose (0, sz - 1)) (T3.size tensor)

tensor3Flip :: TestTree
tensor3Flip = QC.testProperty "Flip Tensor3 value" $ forAll testValues testResult
  where testValues = do
          tensor <- arbitrary `suchThat` ((/= 0) . product . T3.size)
          i <- genI3 tensor
          return (tensor, i)
        testResult (tensor, i) = value == not value'
          where value = tensor T3.! i
                tensor' = T3.update tensor [(i, not value)]
                value' = tensor' T3.! i

tensor3InvalidIndex :: TestTree
tensor3InvalidIndex = QC.testProperty "Invalid Tensor3 index" $ \(tensor :: Tensor3 ()) -> isBottom $ tensor T3.! T3.size tensor

tensor3InvalidUpdate :: TestTree
tensor3InvalidUpdate = QC.testProperty "Invalid Tensor3 update" $ \(tensor :: Tensor3 ()) -> isBottom $ T3.update tensor [(T3.size tensor, ())]

--
-- Simulation tests
--

instance Arbitrary ExecState where
  arbitrary = do
    r <- getSize
    let size = fromIntegral (r + 1)
        empty = T3.create (V.replicate (product size) False) size
        maybeFilled = T3.slice empty (V3 1 0 1, fromIntegral (r - 1))
    filled <- filter snd <$> mapM (\i -> (i, ) <$> arbitrary) maybeFilled
    let model = T3.update empty filled
    return $ initialState model

instance Arbitrary Axis where
  arbitrary = arbitraryBoundedEnum

genLinearDifference :: Int -> Gen I3
genLinearDifference maxLen = mkLinearDifference <$> arbitrary <*> choose (0, maxLen)

genLongDifference :: Gen I3
genLongDifference = genLinearDifference maxLLD

genShortDifference :: Gen I3
genShortDifference = genLinearDifference maxSLD

testModel :: Model
testModel = T3.create
            (V.fromList
              [ False, False, False
              , False, False, False
              , False, False, False

              , False, True, True
              , False, True, True
              , False, False, False

              , False, True, True
              , False, True, True
              , False, False, False
              ]) (V3 3 3 3)

simulationTests :: TestTree
simulationTests = testGroup "Simulation Tests" [
    simulationSMove
  , simulationSMoveOutOfBounds
  , simulationLMove
  , simulationLMoveOutOfBounds
  ]

goodStep :: Model -> I3 -> I3 -> Bool
goodStep model start step = T3.inBounds model (start + step) && all (not . (model T3.!)) (linearPath start step)

simulationSMove :: TestTree
simulationSMove = QC.testProperty "SMove" $ within (2 * 10^(6::Int)) $ forAll testValues $
  \(state, step) -> isJust $ stepState state (M.singleton 1 $ SMove step)
  where testValues = do
          state <- arbitrary
          step <- genLongDifference `suchThat` goodStep (stateMatrix state) 0
          return (state, step)

simulationSMoveOutOfBounds :: TestTree
simulationSMoveOutOfBounds = QC.testProperty "SMove: out of bounds" $ within (2 * 10^(6::Int)) $ forAll testValues $
  \(state, step) -> isNothing $ stepState state (M.singleton 1 $ SMove step)
  where testValues = do
          state <- arbitrary
          step <- mkLinearDifference <$> arbitrary <*> pure (maximum $ T3.size $ stateMatrix state)
          return (state, step)

simulationLMove :: TestTree
simulationLMove = QC.testProperty "LMove" $ within (2 * 10^(6::Int)) $ forAll testValues $
  \(state, d1, d2) -> isJust $ stepState state (M.singleton 1 $ LMove d1 d2)
  where testValues = do
          state <- arbitrary
          d1 <- genShortDifference `suchThat` goodStep (stateMatrix state) 0
          d2 <- genShortDifference `suchThat` goodStep (stateMatrix state) d1
          return (state, d1, d2)

simulationLMoveOutOfBounds :: TestTree
simulationLMoveOutOfBounds = QC.testProperty "LMove: out of bounds" $ within (2 * 10^(6::Int)) $ forAll testValues $
  \(state, d1, d2) -> isNothing $ stepState state (M.singleton 1 $ LMove d1 d2)
  where testValues = do
          state <- arbitrary
          d1 <- mkLinearDifference <$> arbitrary <*> pure (maximum $ T3.size $ stateMatrix state)
          return (state, d1, -d1)

--
-- A* tests
--

immediateNeighbours :: Model -> I3 -> [(I3, I3)]
immediateNeighbours model p = filter (\(i, _) -> T3.inBounds model i && not (model T3.! i)) $ map (\step -> (p + step, step)) allNeighbours
  where allNeighbours =
          [ V3 (-1) 0    0
          , V3 1    0    0
          , V3 0    (-1) 0
          , V3 0    1    0
          , V3 0    0    (-1)
          , V3 0    0    1
          ]

immediateAStar :: Model -> I3 -> I3 -> Maybe [(I3, I3)]
immediateAStar model = aStar (immediateNeighbours model) (\a b -> mlen (a - b))

checkPath :: Model -> I3 -> I3 -> [(I3, I3)] -> Bool
checkPath _ _ _ [] = False
checkPath model start finish path0@((first, _):_)
  | first /= start = False
  | otherwise = not (model T3.! start) && go path0
  where go [] = error "checkPath: impossible"
        go [(current, step)] = current + step == finish
        go ((current, step):path@((next, _):_)) = isNeighbour && not obstructed && followsSteps && go path
          where isNeighbour = next `elem` map fst (immediateNeighbours model current)
                obstructed = model T3.! next
                followsSteps = current + step == next

aStarTests :: TestTree
aStarTests = testGroup "A* Tests" [aStarRandom, aStarGuaranteed]

aStarRandom :: TestTree
aStarRandom = QC.testProperty "A* Random Models Passable" $ within (2 * 10^(6::Int)) $ forAll (arbitrary `suchThat` suitableModel) testPassable
  where suitableModel model = product (T3.size model) /= 0 && not (model T3.! start)
        start = 0
        testPassable model =
          case immediateAStar model start finish of
            Nothing -> True
            Just path -> checkPath model start finish path
          where finish = T3.size model - 1

aStarGuaranteed :: TestTree
aStarGuaranteed = HU.testCase "A* Finds A Path" $ checkPath testModel start finish (fromJust $ immediateAStar testModel start finish) @?= True
  where start = 0
        finish = (T3.size testModel - 1)

packTests :: TestTree
packTests = testGroup "Pack Tests" [
    -- emptyModelPackMove
--   , nonEmptyModelPackMove
  ]


{-testPackMove :: SingleBotModel -> VolatileCoordinate -> VolatileCoordinate -> Bool
testPackMove m0 c c' = (isRight result) && (fromRight c (singleBotPos <$> result) == c') where
  m = m0 {singleBotPos = c}
  simulateStep' em cmd = em >>= \m' -> simulateStep m' cmd
  cmds = map snd $ fromMaybe [] $ aStar (neighbours $ filledModel m) mlenMetric c c'
  result = foldl simulateStep' (Right m) cmds-}

{-emptyModelPackMove :: TestTree
emptyModelPackMove = QC.testProperty "emptyModelPackMove" $ within (2 * 10^(6::Int)) $ \(
    EmptySingleBotModel m0
  , VolatileCoordinateWrapper c
  , VolatileCoordinateWrapper c'
  ) -> testPackMove m0 c c'-}


-- nonEmptyModelPackMove :: TestTree
-- nonEmptyModelPackMove = QC.testProperty "nonEmptyModelPackMove" $ \(
--     NonEmptySingleBotModel m0
--   , VolatileCoordinateWrapper c
--   , VolatileCoordinateWrapper c'
--   ) -> testPackMove m0 c c'
