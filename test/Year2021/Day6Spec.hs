module Year2021.Day6Spec
  ( spec,
  )
where

import Control.Monad.Fix (fix)
import qualified Data.Sequence as Seq
import Test.Hspec (Spec, describe, it, shouldBe)
import Year2021.Day6

spec :: Spec
spec =
  describe "solutions" $ do
    it "populationCount" $ do
      fix populationCount (Day 5) (SpawnAfter 3) `shouldBe` 2
    it "totalPopulation" $ do
      totalPopulation (Day 18) (SpawnAfter <$> [3, 4, 3, 1, 2]) `shouldBe` 26
      totalPopulation (Day 80) (SpawnAfter <$> [3, 4, 3, 1, 2]) `shouldBe` 5934
    it "totalPopulation2" $ do
      totalPopulation2 (Day 18) (SpawnAfter <$> [3, 4, 3, 1, 2]) `shouldBe` 26
      totalPopulation2 (Day 80) (SpawnAfter <$> [3, 4, 3, 1, 2]) `shouldBe` 5934
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 379414
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 1705008653296
