module Year2021.Day7Spec
  ( spec,
  )
where

import qualified Data.Sequence as Seq
import Test.Hspec (Spec, describe, it, shouldBe)
import Year2021.Day7

spec :: Spec
spec =
  describe "solutions" $ do
    it "calculateBestDistance" $ do
      calculateBestDistance [16, 1, 2, 0, 4, 2, 7, 1, 2, 14] `shouldBe` Just (2, 37)
    it "sumToN" $ do
      sumToN 3 `shouldBe` 6
      sumToN 10 `shouldBe` 55
      sumToN 11 `shouldBe` 66
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` Just (354, 349812)

    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` Just (488, 99763899)
