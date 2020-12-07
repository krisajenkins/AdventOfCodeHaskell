module Year2020.Day7Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2020.Day7

spec :: Spec
spec =
  describe "solutions" $ do
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 155
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 54803
