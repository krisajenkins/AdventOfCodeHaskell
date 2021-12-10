module Year2021.Day2Spec
  ( spec,
  )
where

import qualified Data.Sequence as Seq
import Test.Hspec (Spec, describe, it, shouldBe)
import Year2021.Day2

spec :: Spec
spec =
  describe "solutions" $ do
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 1654760
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 1956047400
