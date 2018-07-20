{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Data.Vector as V
import Linear.V3 (V3(..))
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.ChasingBottoms

import ICFPC2018.Types
import ICFPC2018.Tensor3 (Tensor3, Tensor3Idx)
import ICFPC2018.Scoring
import qualified ICFPC2018.Tensor3 as T3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [tensor3Tests]

tensor3Tests :: TestTree
tensor3Tests = testGroup "Tensor3 Tests" [tensor3Flip, tensor3InvalidIndex, tensor3InvalidUpdate, testScoring]

instance Arbitrary a => Arbitrary (Tensor3 a) where
  arbitrary = do
    size <- V3 <$> getSize <*> getSize <*> getSize
    values <- vectorOf (product size) arbitrary
    return $ T3.create (V.fromList values) size

genT3Index :: Tensor3 a -> Gen Tensor3Idx
genT3Index tensor = mapM (\sz -> choose (0, sz - 1)) (T3.size tensor)

tensor3Flip :: TestTree
tensor3Flip = QC.testProperty "Flip Tensor3 value" $ do
  tensor <- arbitrary `suchThat` ((/= 0) . product . T3.size)
  i <- genT3Index tensor
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
    scoreCommand model (Fill (V3 2 1 1)) == 6,
    scoreCommand model (Fill (V3 0 0 0)) == 12,
    scoreTrace model [
      V.fromList [Wait,                        Flip, SMove (V3 0 2 1), Fill (V3 2 1 1), Halt],
      V.fromList [LMove (V3 0 2 1) (V3 1 0 0), Wait, Flip,             Fill (V3 0 0 0), Halt]
      ] == sum [sum [0, 0, 6, 6, 0], sum [10, 0, 0, 12, 0]]
    ]