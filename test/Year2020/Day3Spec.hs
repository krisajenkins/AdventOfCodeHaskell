{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day3Spec
  ( spec
  ) where

import qualified Data.Sequence   as Seq
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (property, (===))
import           Year2020.Day3

spec :: Spec
spec =
  describe "solutions" $ do
    it "rotate" $ rotate 3 [1, 2, 3, 4, 5] `shouldBe` [4, 5, 1, 2, 3]
    it "rotate" $ rotate 9 [1, 2, 3, 4, 5] `shouldBe` [5, 1, 2, 3, 4]
    it "rotate preserves length" $
      property $ \(xs :: [Int]) -> length (rotate 3 xs) === length xs
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 254
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 1666768320
