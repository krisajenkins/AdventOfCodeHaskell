module Year2020.Day2Spec
  ( spec
  ) where

import qualified Data.Sequence as Seq
import           Test.Hspec    (Spec, describe, it, shouldBe)
import           Year2020.Day2

spec :: Spec
spec =
  describe "solutions" $ do
    it "Parser" $ do
      first <- head <$> readSequence
      first `shouldBe`
        Entry {lowerBound = 3, upperBound = 4, check = 'j', password = "tjjj"}
    it "isValidPosition" $ do
      True `shouldBe`
        isValidPosition
          (Entry
             {lowerBound = 1, upperBound = 3, check = 'a', password = "abcde"})
      False `shouldBe`
        isValidPosition
          (Entry
             {lowerBound = 1, upperBound = 3, check = 'b', password = "cdefg"})
      False `shouldBe`
        isValidPosition
          (Entry
             { lowerBound = 2
             , upperBound = 9
             , check = 'c'
             , password = "ccccccccc"
             })
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 483
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 482
