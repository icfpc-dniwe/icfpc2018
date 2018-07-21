{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map as M
import Linear.V3 (V3(..))
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.ChasingBottoms
import Data.Either (isLeft, isRight)
import Linear.Vector ((*^))

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Tensor3 (Tensor3, I3)
import ICFPC2018.Scoring
import ICFPC2018.Model
import ICFPC2018.Simulation
import qualified ICFPC2018.Tensor3 as T3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ tensor3Tests
  , simulationTests
  , aStarTests
  ]

--
-- Tensor3 Tests
--

tensor3Tests :: TestTree
tensor3Tests = testGroup "Tensor3 Tests" [tensor3Flip, tensor3InvalidIndex, tensor3InvalidUpdate, testScoring]

instance Arbitrary a => Arbitrary (Tensor3 a) where
  arbitrary = do
    let getSize' = choose (0, 32)
    size <- V3 <$> getSize' <*> getSize' <*> getSize'
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
  
testScoring :: TestTree
testScoring = testGroup "Scoring for commands"
  [ HU.testCase "Halt" $ scoreOne Halt @?= 0
  , HU.testCase "Wait" $ scoreOne Wait @?= 0
  , HU.testCase "Flip" $ scoreOne Flip @?= 0
  , HU.testCase "SMove" $ scoreOne (SMove (V3 0 2 1)) @?= 2 * 3
  , HU.testCase "LMove" $ scoreOne (LMove (V3 0 2 1) (V3 1 0 0)) @?= 2 * (2 + 1 + 2)
  , HU.testCase "Fission" $ scoreOne (Fission (V3 1 1 1) 1) @?= 24
  , HU.testCase "FusionP" $ scoreOne (FusionP (V3 1 1 1)) @?= -24
  , HU.testCase "FusionS" $ scoreOne (FusionS (V3 1 1 1)) @?= 0
  , HU.testCase "Fill empty" $ scoreOne (Fill (V3 0 0 0)) @?= 12
  , HU.testCase "Fill again" $ scoreOne (Fill (V3 2 1 1)) @?= 6
  , HU.testCase "scoreTrace" $ scoreTrace testModel testTrace @?= testTraceScore
  ]
  where scoreOne = scoreCommand testModel
        testTrace = M.fromList <$> zip [0, 1] <$>
                    [ [Wait, LMove (V3 0 2 1) (V3 1 0 0)]
                    , [Flip, Wait]
                    , [SMove (V3 0 2 1), Flip]
                    , [Fill (V3 2 1 1), Fill (V3 0 0 0)]
                    , [Halt, Halt]
                    ]
        testTraceScore = sum [sum [0, 0, 6, 12 {-6-}, 0], sum [10, 0, 0, 12, 0]] + 20 * 2 * 5 + (3 + 3 + 30 + 3 + 3) * 27

--
-- Simulation tests
--

simulationTests :: TestTree
simulationTests = testGroup "Simulation Tests" [
    simulationSMove
  , simulationSMoveOutOfBounds
  , simulationSMoveNonVoid
  , simulationLMove
  , simulationLMoveOutOfBounds
  , simulationLMoveNonVoid
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

newtype ShortDifferenceWrapper = ShortDifferenceWrapper ShortDifference deriving Show
instance Arbitrary ShortDifferenceWrapper where
  arbitrary = ShortDifferenceWrapper <$> (mkLinearDifference <$> arbitrary <*> (choose (1, maxSLD)))


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


simulationLMove :: TestTree
simulationLMove = QC.testProperty "LMove" $ \(
    EmptySingleBotModel m
  , VolatileCoordinateWrapper c
  , ShortDifferenceWrapper d1, ShortDifferenceWrapper d2
  ) -> isBounded m (c+d1) && isBounded m (c+d1+d2) ==> isRight $ simulateStep m {botPos = c} (LMove d1 d2)


simulationLMoveOutOfBounds :: TestTree
simulationLMoveOutOfBounds = QC.testProperty "LMove: out of bounds" $ \(
    EmptySingleBotModel m
  , VolatileCoordinateWrapper c
  , axis
  , dir
  ) -> let
    l = maximum (T3.size $ filledModel m)
    d = mkLinearDifference axis ((if dir then 1 else (-1)) * l)
    in isLeft $ simulateStep m {botPos = c} (LMove d (-d))


simulationLMoveNonVoid :: TestTree
simulationLMoveNonVoid = QC.testProperty "LMove: non void" $ \(
    EmptySingleBotModel m
  , VolatileCoordinateWrapper c
  , ShortDifferenceWrapper d1, ShortDifferenceWrapper d2
  , NonEmpty ps1, NonEmpty ps2
  ) -> let
    l1 = mlen d1 + 1
    l2 = mlen d2 + 1
    n1 = normalizeLinearDifference d1
    n2 = normalizeLinearDifference d2
    ps1' = map (`mod` l1) ps1
    ps2' = map (`mod` l2) ps2
    updates1 = zip (map (\i -> c + i*^n1) ps1') (repeat True)
    updates2 = zip (map (\i -> c + d1 + i*^n2) ps2') (repeat True)
    m' = m {
        botPos = c
      , filledModel = T3.update (filledModel m) (updates1 ++ updates2)
      }
    in isBounded m (c+d1) && isBounded m (c+d1+d2) ==>
       isLeft $ simulateStep m' (LMove d1 d2)

--
-- A* tests
--

immediateNeighbours :: Model -> I3 -> [(I3, I3)]
immediateNeighbours model p = filter (\(i, _) -> checkBounds (T3.size model) i && not (model T3.! i)) $ map (\step -> (p + step, step)) allNeighbours
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
