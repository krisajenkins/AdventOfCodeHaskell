module Year2019.Day1Spec
  ( spec
  ) where

import qualified Data.Sequence as Seq
import           Test.Hspec    (Spec, describe, it, shouldBe)
import           Year2019.Day1

spec :: Spec
spec =
  describe "solutions" $ do
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 3405637
    it "totalFullFuelRequirement" $ do
      totalFullFuelRequirement [1969] `shouldBe` 966
      totalFullFuelRequirement [100756] `shouldBe` 50346
    it "Part 2" $ do
      answer1 <- solution2
      answer1 `shouldBe` 5105597
