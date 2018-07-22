{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad
import Data.Maybe
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as M
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
  ]

--
-- Tensor3 Tests
--

tensor3Tests :: TestTree
tensor3Tests = testGroup "Tensor3 tests" [tensor3Flip, tensor3InvalidIndex, tensor3InvalidUpdate]

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

runOnMatrix :: Model -> [[(Int, Command)]] -> Maybe ExecState
runOnMatrix matrix = foldM stepState ((initialState r) { stateMatrix = matrix }) . map M.fromList
  where V3 r _ _ = T3.size matrix

runEmptyMatrix :: [[(Int, Command)]] -> Maybe ExecState
runEmptyMatrix = runOnMatrix emptyModel

runTestMatrix :: [[(Int, Command)]] -> Maybe ExecState
runTestMatrix = runOnMatrix testModel

simulationTests :: TestTree
simulationTests = testGroup "Simulation tests" [
    simulationSMove
  , simulationLMove
  , simulationSMoveErrors
  , simulationLMoveErrors
  ]

goodStep :: Model -> I3 -> I3 -> Bool
goodStep model start step = T3.inBounds model (start + step) && all (not . (model T3.!)) (linearPath start step)

simulationSMove :: TestTree
simulationSMove = QC.testProperty "SMove" $ forAll testValues $
  \(state, step) -> isJust $ stepState state (M.singleton 1 $ SMove step)
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
  where failEmpty name steps = HU.testCase name $ runEmptyMatrix steps @?= Nothing
        failTest name steps = HU.testCase name $ runTestMatrix steps @?= Nothing

simulationLMove :: TestTree
simulationLMove = QC.testProperty "LMove" $ forAll testValues $
  \(state, d1, d2) -> isJust $ stepState state (M.singleton 1 $ LMove d1 d2)
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
  where failEmpty name steps = HU.testCase name $ runEmptyMatrix steps @?= Nothing
        failTest name steps = HU.testCase name $ runTestMatrix steps @?= Nothing

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
  \(state, p) -> isJust $ foldM stepState state $ map (M.singleton 1) $ packMove 0 p
  where testValues = do
          state <- genEmptyExecState
          p <- genI3 $ stateMatrix state
          return (state, p)

testSingleBotPackIntensions :: Intensions -> ExecState -> Bool
testSingleBotPackIntensions intensions state =
  isJust $ foldM stepState state $ packSingleBotIntensions (stateMatrix state) 1 (botPos $ stateBots state M.! 1) intensions

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
  in M.size (last commands) == 1

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
