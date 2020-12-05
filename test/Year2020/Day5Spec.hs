module Year2020.Day5Spec
  ( spec
  ) where

import           Data.Bits
import qualified Data.Sequence as Seq
import           Test.Hspec    (Spec, describe, it, shouldBe)
import           Year2020.Day5

spec :: Spec
spec =
  describe "solutions" $ do
    it "Decode Boarding Pass" $ do
      ((zeroBits `setBit` 0)) `shouldBe` (1 :: Int)
      (shift (zeroBits `setBit` 0) 2) `shouldBe` (4 :: Int)
      decodeBoardingPass "BFFFBBFRRR" `shouldBe`
        Seat {row = 70, column = 7, seatId = 567}
      decodeBoardingPass "FFFBBBFRRR" `shouldBe`
        Seat {row = 14, column = 7, seatId = 119}
      decodeBoardingPass "BBFFBBFRLL" `shouldBe`
        Seat {row = 102, column = 4, seatId = 820}
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` Seat {row = 124, column = 4, seatId = 996}
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` Just 671
