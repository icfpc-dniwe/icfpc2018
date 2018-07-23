{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad
import Data.Maybe
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.IntSet as IS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Linear.V3 (V3(..))
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.ChasingBottoms
-- import Data.List (dropWhileEnd)
-- import Text.Heredoc

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Tensor3 (Tensor3, I3)
import ICFPC2018.Simulation
import ICFPC2018.Model
import ICFPC2018.Pack
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Pipeline
import qualified ICFPC2018.Solvers.HighSolver as High
import ICFPC2018.Prepare

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = adjustOption (min 16 :: QC.QuickCheckMaxSize -> QC.QuickCheckMaxSize) $ testGroup "Tests"
  [ tensor3Tests
  , simulationTests
  , aStarTests
  , packTests
  , solverTests
  --, floodFillTests
  --, preparationTests
  ]

--
-- Tensor3 Tests
--

tensor3Tests :: TestTree
tensor3Tests = testGroup "Tensor3 tests" [ tensor3Flip
                                         , tensor3InvalidIndex
                                         , tensor3InvalidUpdate
                                         , tensor3ViewSize
                                         , tensor3ViewIndex
                                         --, tensor3ViewUpdate
                                         , tensor3Fill
                                         , tensor3FillBox]

instance (Unbox a, Arbitrary a) => Arbitrary (Tensor3 a) where
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

testViewTensor :: Tensor3 Bool
testViewTensor = T3.replicate (V3 3 3 3) False `T3.update` [(V3 1 1 1, True)]

testViewView :: Tensor3 Bool
testViewView = testViewTensor `T3.slice` (V3 1 1 1, V3 2 2 2)

tensor3ViewSize :: TestTree
tensor3ViewSize = HU.testCase "Tensor3 view size" $ T3.size testViewView @?= (V3 2 2 2)

tensor3ViewIndex :: TestTree
tensor3ViewIndex = HU.testCase "Tensor3 view index" $ testViewView T3.! (V3 0 0 0) @?= True
{-
tensor3ViewUpdate :: TestTree
tensor3ViewUpdate = HU.testCase "Tensor3 view update" $ testViewView `T3.update` [(V3 0 0 0, False)] @?= T3.slice (T3.replicate (V3 3 3 3) False) (V3 1 1 1, V3 2 2 2)
-}
testFillModelFalse :: Model
testFillModelFalse = T3.replicate (V3 3 3 3) False

testFillModelTrue :: Model
testFillModelTrue = T3.replicate (V3 3 3 3) True

testFillModelBox :: Model
testFillModelBox = testFillModelFalse `T3.update` [ (V3 1 1 1, True), (V3 1 1 2, True)
                                                  , (V3 1 2 1, True), (V3 1 2 2, True)
                                                  , (V3 2 1 1, True), (V3 2 1 2, True)
                                                  , (V3 2 2 1, True), (V3 2 2 2, True)
                                                  ]

tensor3Fill :: TestTree
tensor3Fill = HU.testCase "Fill whole tensor" $ testFillModelFalse `T3.fill` True @?= testFillModelTrue

tensor3FillBox :: TestTree
tensor3FillBox = HU.testCase "Fill subtensor" $ T3.fillBox testFillModelFalse (V3 1 1 1, V3 2 2 2) True @?= testFillModelBox

--
-- Simulation tests
--

genEmptyExecState :: Gen ExecState
genEmptyExecState = do
  r <- (+ 3) <$> getSize
  return $ initialState r

genModel :: Gen Model
genModel = do
  r <- (+ 3) <$> getSize
  let size = fromIntegral r
      empty = T3.create (V.replicate (product size) False) size
      maybeFilled = boxIndices (V3 1 0 1) (fromIntegral (r - 2))
  filled <- filter snd <$> mapM (\i -> (i, ) <$> arbitrary) maybeFilled
  return $ T3.update empty filled

instance Arbitrary ExecState where
  arbitrary = do
    m <- genModel
    let V3 r _ _ = T3.size m
    return $ (initialState r) { stateMatrix = m }

instance Arbitrary T3.Axis where
  arbitrary = arbitraryBoundedEnum

genLinearDifference :: Int -> Gen I3
genLinearDifference maxLen = mkLinearDifference <$> arbitrary <*> choose (0, maxLen)

genLongDifference :: Gen I3
genLongDifference = genLinearDifference maxLLD

genShortDifference :: Gen I3
genShortDifference = genLinearDifference maxSLD

emptyModel :: Model
emptyModel = T3.create (V.replicate (product size) False) size
  where size = V3 16 16 16

-- 3x2x2 cuboid
testModel :: Model
testModel = T3.create
            (V.fromList
              [ False, False, False, False
              , False, False, False, False
              , False, False, False, False
              , False, False, False, False

              , False, True, True, False
              , False, True, True, False
              , False, True, True, False
              , False, False, False, False

              , False, True, True, False
              , False, True, True, False
              , False, True, True, False
              , False, False, False, False

              , False, False, False, False
              , False, False, False, False
              , False, False, False, False
              , False, False, False, False
              ]) (V3 4 4 4)

runOnMatrix :: Model -> ExecState -> [[(Int, Command)]] -> Maybe ExecState
runOnMatrix matrix initState = foldM stepState (initState { stateMatrix = matrix }) . map IM.fromList

runOnMatrix' :: Model -> [[(Int, Command)]] -> Maybe ExecState
runOnMatrix' matrix = runOnMatrix matrix $ initialState r where
  V3 r _ _ = T3.size matrix

runEmptyMatrix :: ExecState -> [[(Int, Command)]] -> Maybe ExecState
runEmptyMatrix initState = runOnMatrix emptyModel initState

runEmptyMatrix' :: [[(Int, Command)]] -> Maybe ExecState
runEmptyMatrix' = runOnMatrix' emptyModel

runTestMatrix :: ExecState -> [[(Int, Command)]] -> Maybe ExecState
runTestMatrix initState = runOnMatrix testModel initState

runTestMatrix' :: [[(Int, Command)]] -> Maybe ExecState
runTestMatrix' = runOnMatrix' testModel

simulationTests :: TestTree
simulationTests = testGroup "Simulation tests" [
    simulationSMove
  , simulationLMove
  , simulationSMoveErrors
  , simulationLMoveErrors
  , simulationGFillTests
  ]

goodStep :: Model -> I3 -> I3 -> Bool
goodStep model start step = T3.inBounds model (start + step) && all (not . (model T3.!)) (linearPath start step)

simulationSMove :: TestTree
simulationSMove = QC.testProperty "SMove" $ forAll testValues $
  \(state, step) -> isJust $ stepState state (IM.singleton 1 $ SMove step)
  where testValues = do
          state <- arbitrary
          step <- genLongDifference `suchThat` goodStep (stateMatrix state) 0
          return (state, step)

simulationSMoveErrors :: TestTree
simulationSMoveErrors = testGroup "SMove edge cases"
  [ failEmpty "SMove: too far" [[(1, SMove (V3 (maxLLD + 1) 0 0))]]
  , failEmpty "SMove: out of bounds" [[(1, SMove (V3 (-1) 0 0))]]
  , failTest "SMove: collision" [[(1, SMove (V3 1 0 0))], [(1, SMove (V3 0 0 1))]]
  ]
  where failEmpty name steps = HU.testCase name $ runEmptyMatrix' steps @?= Nothing
        failTest name steps = HU.testCase name $ runTestMatrix' steps @?= Nothing

simulationLMove :: TestTree
simulationLMove = QC.testProperty "LMove" $ forAll testValues $
  \(state, d1, d2) -> isJust $ stepState state (IM.singleton 1 $ LMove d1 d2)
  where testValues = do
          state <- arbitrary
          d1 <- genShortDifference `suchThat` goodStep (stateMatrix state) 0
          d2 <- genShortDifference `suchThat` goodStep (stateMatrix state) d1
          return (state, d1, d2)

simulationLMoveErrors :: TestTree
simulationLMoveErrors = testGroup "LMove edge cases"
  [ failEmpty "LMove: first too far" [[(1, LMove (V3 (maxSLD + 1) 0 0) (V3 0 1 0))]]
  , failEmpty "LMove: second too far" [[(1, LMove (V3 0 1 0) (V3 (maxSLD + 1) 0 0))]]
  , failEmpty "LMove: out of bounds" [[(1, LMove (V3 (-1) 0 0) (V3 1 0 0))]]
  , failTest "LMove: collision" [[(1, LMove (V3 1 0 0) (V3 0 0 1))]]
  ]
  where failEmpty name steps = HU.testCase name $ runEmptyMatrix' steps @?= Nothing
        failTest name steps = HU.testCase name $ runTestMatrix' steps @?= Nothing

-- GFill tests
-- RNBMX means N regions, M bots, Pass/Fail
data GFillTestPrimitive = R1B2P | R1B4P | R1B8P | R2B4P | R2B8P | R2B16P | R1B2F | R1B4F | R1B8F | R2B4F | R2B8F | R2B16F | R1B7F
                        deriving (Show, Eq)

genBot :: I3 -> BotState
genBot pos = BotState { botPos = pos, botSeeds = IS.empty }

initialGFillBots :: GFillTestPrimitive -> [BotState]
initialGFillBots R1B2P  = genBot <$> [V3 0 0 0, V3 0 0 4]
initialGFillBots R1B4P  = genBot <$> [V3 0 0 0, V3 4 0 0, V3 4 0 4, V3 0 0 4]
initialGFillBots R1B8P  = genBot <$> [V3 0 0 0, V3 4 0 0, V3 4 0 4, V3 0 0 4, V3 0 4 0, V3 4 4 0, V3 4 4 4, V3 0 4 4]
initialGFillBots R2B4P  = genBot <$> [V3 0 0 0, V3 0 0 4, V3 4 3 0, V3 0 3 0]
initialGFillBots R2B8P  = genBot <$> [V3 0 0 0, V3 4 0 0, V3 4 0 4, V3 0 0 4, V3 5 1 0, V3 5 5 0, V3 5 5 5, V3 5 1 5]
initialGFillBots R2B16P = genBot <$> [V3 0 0 0, V3 4 0 0, V3 4 0 4, V3 0 0 4, V3 0 4 0, V3 4 4 0, V3 4 4 4, V3 0 4 4,
                                      V3 6 6 6, V3 9 6 6, V3 9 6 9, V3 6 6 9, V3 6 9 6, V3 9 9 6, V3 9 9 9, V3 6 9 9]
initialGFillBots R1B2F  = genBot <$> [V3 1 0 0, V3 0 0 4]
initialGFillBots R1B4F  = genBot <$> [V3 0 0 0, V3 4 0 0, V3 4 0 3, V3 0 0 4]
initialGFillBots R1B8F  = genBot <$> [V3 0 0 0, V3 4 0 0, V3 4 0 4, V3 0 0 4, V3 0 2 0, V3 4 4 0, V3 4 5 4, V3 0 4 4]
initialGFillBots R2B4F  = genBot <$> [V3 0 0 0, V3 0 0 4, V3 2 0 0, V3 0 0 6]
initialGFillBots R2B8F  = genBot <$> [V3 0 4 0, V3 4 4 0, V3 4 4 4, V3 0 4 4, V3 2 0 0, V3 2 4 0, V3 2 4 4, V3 2 0 4]
initialGFillBots R2B16F = genBot <$> [V3 0 0 0, V3 4 0 0, V3 4 0 4, V3 0 0 4, V3 0 4 0, V3 4 4 0, V3 4 4 4, V3 0 4 4,
                                      V3 2 2 2, V3 6 2 2, V3 6 2 6, V3 2 2 6, V3 2 6 2, V3 6 6 2, V3 6 6 6, V3 2 6 6]
initialGFillBots R1B7F  = genBot <$> [V3 0 0 0, V3 4 0 0, V3 4 0 4, V3 0 0 4, V3 5 1 0, V3 5 5 0, V3 5 5 1]

genGFillStep' :: [Command] -> Step
genGFillStep' cmds = IM.fromList $ zip [1..] cmds

genGFillStep :: GFillTestPrimitive -> Step
genGFillStep R1B2P  = genGFillStep' [GFill (V3   0    0    1 ) (V3   0    0    2 ),
                                     GFill (V3   0    0  (-1)) (V3   0    0  (-2))
                                    ]
genGFillStep R1B4P  = genGFillStep' [GFill (V3   0    1    0 ) (V3   4    0    4 ),
                                     GFill (V3   0    1    0 ) (V3 (-4)   0    4 ),
                                     GFill (V3   0    1    0 ) (V3 (-4)   0  (-4)),
                                     GFill (V3   0    1    0 ) (V3   4    0  (-4))
                                    ]
genGFillStep R1B8P  = genGFillStep' [GFill (V3   1    0    1 ) (V3   2    4    2 ),
                                     GFill (V3 (-1)   0    1 ) (V3 (-2)   4    2 ),
                                     GFill (V3 (-1)   0  (-1)) (V3 (-2)   4  (-2)),
                                     GFill (V3   1    0  (-1)) (V3   2    4  (-2)),
                                     GFill (V3   1    0    1 ) (V3   2  (-4)   2 ),
                                     GFill (V3 (-1)   0    1 ) (V3 (-2) (-4)   2 ),
                                     GFill (V3 (-1)   0  (-1)) (V3 (-2) (-4) (-2)),
                                     GFill (V3   1    0  (-1)) (V3   2  (-4) (-2))
                                    ]
genGFillStep R2B4P  = genGFillStep' [GFill (V3   0    1    0 ) (V3   0    0    4 ),
                                     GFill (V3   0    1    0 ) (V3   0    0  (-4)),
                                     GFill (V3   0  (-1)   0 ) (V3 (-4)   0    0 ),
                                     GFill (V3   0  (-1)   0 ) (V3   4    0    0 )
                                    ]
genGFillStep R2B8P  = genGFillStep' [GFill (V3   1    0    1 ) (V3   2    0    2 ),
                                     GFill (V3 (-1)   0    1 ) (V3 (-2)   0    2 ),
                                     GFill (V3 (-1)   0  (-1)) (V3 (-2)   0  (-2)),
                                     GFill (V3   1    0  (-1)) (V3   2    0  (-2)),
                                     GFill (V3   1    0    0 ) (V3   0    4    5 ),
                                     GFill (V3   1    0    0 ) (V3   0  (-4)   5 ),
                                     GFill (V3   1    0    0 ) (V3   0  (-4) (-5)),
                                     GFill (V3   1    0    0 ) (V3   0    4  (-5))
                                    ]
genGFillStep R2B16P = genGFillStep' [GFill (V3   1    0    1 ) (V3   2    4    2 ),
                                     GFill (V3 (-1)   0    1 ) (V3 (-2)   4    2 ),
                                     GFill (V3 (-1)   0  (-1)) (V3 (-2)   4  (-2)),
                                     GFill (V3   1    0  (-1)) (V3   2    4  (-2)),
                                     GFill (V3   1    0    1 ) (V3   2  (-4)   2 ),
                                     GFill (V3 (-1)   0    1 ) (V3 (-2) (-4)   2 ),
                                     GFill (V3 (-1)   0  (-1)) (V3 (-2) (-4) (-2)),
                                     GFill (V3   1    0  (-1)) (V3   2  (-4) (-2)),

                                     GFill (V3   1    0    1 ) (V3   1    3    1 ),
                                     GFill (V3 (-1)   0    1 ) (V3 (-1)   3    1 ),
                                     GFill (V3 (-1)   0  (-1)) (V3 (-1)   3  (-1)),
                                     GFill (V3   1    0  (-1)) (V3   1    3  (-1)),
                                     GFill (V3   1    0    1 ) (V3   1  (-3)   1 ),
                                     GFill (V3 (-1)   0    1 ) (V3 (-1) (-3)   1 ),
                                     GFill (V3 (-1)   0  (-1)) (V3 (-1) (-3) (-1)),
                                     GFill (V3   1    0  (-1)) (V3   1  (-3) (-1))
                                    ]
genGFillStep R1B2F  = genGFillStep R1B2P
genGFillStep R1B4F  = genGFillStep R1B4P
genGFillStep R1B8F  = genGFillStep R1B8P
genGFillStep R2B4F  = genGFillStep R2B4P
genGFillStep R2B8F  = genGFillStep R2B8P
genGFillStep R2B16F = genGFillStep R2B16P
genGFillStep R1B7F  = genGFillStep R1B8P

initialGFillState :: IntMap BotState -> ExecState
initialGFillState bots = ExecState {
    stateEnergy = 0
  , stateHarmonics = High
  , stateMatrix = emptyModel
  , stateBots = bots
  , stateGFillDone = False
  , stateGVoidDone = False
  , stateHalted = False
  }

simulationGFillTests :: TestTree
simulationGFillTests = testGroup "GFill edge cases" $ failTests ++ succTests where
  failEmpty name bots steps = HU.testCase name $ runEmptyMatrix (initialGFillState bots) steps @?= Nothing
  succEmpty name bots steps = HU.testCase name $ isJust (runEmptyMatrix (initialGFillState bots) steps) @?= True
  failCases = [R1B2F, R1B4F, R1B8F, R2B4F, R2B8F, R2B16F, R1B7F]
  succCases = [R1B2P, R1B4P, R1B8P, R2B4P, R2B8P, R2B16P]
  failBots = map (\gen -> IM.fromList $ zip [1..] $ initialGFillBots gen) failCases
  succBots = map (\gen -> IM.fromList $ zip [1..] $ initialGFillBots gen) succCases
  failTests = map (\(bots, gen) -> failEmpty ("GFill: " ++ show gen) bots [IM.toList $ genGFillStep gen]) $ zip failBots failCases
  succTests = map (\(bots, gen) -> succEmpty ("GFill: " ++ show gen) bots [IM.toList $ genGFillStep gen]) $ zip succBots succCases

---
--- Preparation Tests
---
{-
splitTestModel :: Model
splitTestModel = T3.fillBox emptyModel (V3 2 0 2, V3 11 15 11) True

splitFirstModel :: Model
splitFirstModel = T3.sliceAxis splitTestModel T3.X 0 6

splitSecondModel :: Model
splitSecondModel = T3.sliceAxis splitTestModel T3.X 7 15

preparationTests :: TestTree
preparationTests = testGroup "Preparation functions" [splitModelTests]

splitModelTests :: TestTree
splitModelTests = HU.testCase "splitting is correct" $ splitModel splitTestModel T3.X @?= (splitFirstModel, splitSecondModel)
-}
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
checkPath _ start finish [] = start == finish
checkPath model start finish path0@(_:_)
  | otherwise = not (model T3.! start) && go start path0
  where go current [] = current == finish
        go current ((pathCurrent, step):path) = isNeighbour && not obstructed && sameCurrent && go next path
          where next = current + step
                isNeighbour = next `elem` map fst (immediateNeighbours model current)
                obstructed = model T3.! next
                sameCurrent = current == pathCurrent

aStarTests :: TestTree
aStarTests = testGroup "A* tests" [aStarRandom, aStarGuaranteed]

pathFindingTime :: Int
pathFindingTime = 5 * 10^(5::Int)

aStarRandom :: TestTree
aStarRandom = QC.testProperty "A* random models passable" $ within pathFindingTime $ forAll (arbitrary `suchThat` suitableModel) testPassable
  where suitableModel model = product (T3.size model) /= 0 && not (model T3.! start)
        start = 0
        testPassable model =
          case immediateAStar model start finish of
            Nothing -> True
            Just path -> checkPath model start finish path
          where finish = T3.size model - 1

aStarGuaranteed :: TestTree
aStarGuaranteed = HU.testCase "A* finds a path" $ checkPath testModel start finish (fromJust $ immediateAStar testModel start finish) @?= True
  where start = 0
        finish = (T3.size testModel - 1)

packTests :: TestTree
packTests = testGroup "Pack tests"
  [ packMoveTest
  , emptySingleIntension
  , nonEmptySingleIntension
  ]

packMoveTest :: TestTree
packMoveTest = QC.testProperty "packMoves is valid" $ within pathFindingTime $ forAll testValues $
  \(state, p) -> isJust $ foldM stepState state $ map (IM.singleton 1) $ packMove 0 p
  where testValues = do
          state <- genEmptyExecState
          p <- genI3 $ stateMatrix state
          return (state, p)

testSingleBotPackIntensions :: Intensions -> ExecState -> Bool
testSingleBotPackIntensions intensions state =
  isJust $ foldM stepState state $ packSingleBotIntensions (stateMatrix state) 1 (botPos $ stateBots state IM.! 1) intensions

genFillablePoint :: ExecState -> Gen I3
genFillablePoint state = do
  let V3 r _ _ = T3.size $ stateMatrix state
  V3 <$> choose (1, r - 2) <*> choose (0, r - 2) <*> choose (1, r - 2)

emptySingleIntension :: TestTree
emptySingleIntension = QC.testProperty "Single fill on an empty matrix" $ within pathFindingTime $ forAll testValues $
  \(state, p) -> testSingleBotPackIntensions [FillIdx p] state
  where testValues = do
          state <- genEmptyExecState
          p <- genFillablePoint state
          return (state, p)

nonEmptySingleIntension :: TestTree
nonEmptySingleIntension = QC.testProperty "Single fill on a filled matrix" $ within pathFindingTime $
  \state -> testSingleBotPackIntensions [FillIdx (T3.size (stateMatrix state) - 2)] state

solverTests :: TestTree
solverTests = testGroup "Solvers tests"
  [ solverTest "High" $ \state model -> packSingleBotIntensions (stateMatrix state) 1 0 $ High.solver model
  , solverTest "Pipeline" $ \_ model -> pipeline model
  ]

solverTest :: String -> (ExecState -> Model -> Trace) -> TestTree
solverTest name solver = QC.testProperty (name ++ " solver") $ within (4 * pathFindingTime) $ forAll genModel $ \model ->
  let V3 r _ _ = T3.size model
      state0 = initialState r
      commands = solver state0 model
  -- in isJust $ foldM debugState state0 $ traceShow (r, length commands) commands
  in IM.size (last commands) == 1

{-
floodFillTests :: TestTree
floodFillTests = testGroup "FloodFill Tests" [
    mkFloodFillTest "t1"
      [here|
        000
        000
        000|]
      [(V3 0 0 0, 1 :: Int)]
      [here|
        111
        111
        111|]

  , mkFloodFillTest "t2"
      [here|
        01000
        01010
        01010
        01010
        00010|]
      [ (V3 0 0 0, 2 :: Int)
      , (V3 1 0 0, 3 :: Int)
      , (V3 3 0 1, 4 :: Int)]
      [here|
        23222
        23242
        23242
        23242
        22242|]

  , mkFloodFillTest "t3"
      [here|
        00100100
        01001001
        10010010
        00100100
        01001001
        10010010
        00100100
        01001000|]
      [ (V3 7 0 0, 1 :: Int)
      , (V3 7 0 0, 0 :: Int)
      , (V3 7 0 0, 1 :: Int)
      , (V3 7 0 0, 2 :: Int)]
      [here|
        00222222
        02222222
        22222222
        22222222
        22222222
        22222220
        22222200
        22222000|]
  ]


mkFloodFillTest :: (Read a, Show a, Eq a) => String -> String -> [(I3, a)] -> String -> TestTree
mkFloodFillTest name s fs s' = QC.testProperty name
  $ (== cleanup s')
  . cleanup
  . T3.showY 0
  . (\m -> foldl (\m' (idx, v) -> floodFill idx v m') m fs)
  . fmap (read . pure)
  $ T3.scanY (cleanup s) where
  cleanup = dropWhileEnd (== '\n') . dropWhile (=='\n') . filter (/= ' ')
-}
