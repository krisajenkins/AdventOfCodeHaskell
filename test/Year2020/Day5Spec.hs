module Year2020.Day5Spec
  ( spec
  ) where

import Data.Bits
import qualified Data.Sequence as Seq
import Test.Hspec (Spec, describe, it, shouldBe)
import Year2020.Day5

spec :: Spec
spec =
  describe "solutions" $ do
    it "Decode Boarding Pass" $ do
      ((zeroBits `setBit` 0)) `shouldBe` (1 :: Int)
      (shift (zeroBits `setBit` 0) 2) `shouldBe` (4 :: Int)
      decodeBoardingPass "BFFFBBFRRR" `shouldBe` 567
      decodeBoardingPass "FFFBBBFRRR" `shouldBe` 119
      decodeBoardingPass "BBFFBBFRLL" `shouldBe` 820
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 996
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` Just 671
