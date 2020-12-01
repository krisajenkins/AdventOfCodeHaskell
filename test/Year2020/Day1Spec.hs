module Year2020.Day1Spec
  ( spec
  ) where

import qualified Data.Sequence as Seq
import           Test.Hspec    (Spec, describe, it, shouldBe)
import           Year2020.Day1

spec :: Spec
spec =
  describe "solutions" $ do
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` Just 1007104
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` Just 18847752
