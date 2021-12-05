module Day01Spec (spec) where

import Day01
import Test.Hspec

spec :: Spec
spec = do
  let xs =
        [ 199,
          200,
          208,
          210,
          200,
          207,
          240,
          269,
          260,
          263
        ]
  describe "part1" $ do
    it "works with the example data" $ do
      numberOfIncreases xs `shouldBe` 7
  describe "part2" $ do
    it "works with the example data" $ do
      numberOfThreeMeasurementIncreases xs `shouldBe` 5
