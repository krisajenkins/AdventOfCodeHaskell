module Year2020.Day9Spec
  ( spec
  ) where

import qualified Data.Sequence as Seq
import           Test.Hspec    (Spec, describe, it, shouldBe)
import           Year2020.Day9

spec :: Spec
spec =
  describe "solutions" $ do
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` Just 257342611
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` Just 35602097
