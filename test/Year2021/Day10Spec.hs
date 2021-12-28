module Year2021.Day10Spec
  ( spec,
  )
where

import qualified Data.Sequence as Seq
import Test.Hspec (Spec, describe, it, shouldBe)
import Year2021.Day10

spec :: Spec
spec =
  describe "solutions" $ do
    it "Support" $ do
      findCorruption mempty "{([(<{}[<>[]}>{[]{[(<()>" `shouldBe` Just '}'
      findCorruption mempty "[[<[([]))<([[{}[[()]]]" `shouldBe` Just ')'
      findCompletion mempty "[({(<(())[]>[[{[]{<()<>>" `shouldBe` Just (Stack "}}]])})]")
      fmap scoreStack (findCompletion mempty "[({(<(())[]>[[{[]{<()<>>") `shouldBe` Just 288957
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 464991
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 3662008566
