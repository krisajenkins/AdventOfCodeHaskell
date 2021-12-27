module Year2021.Day9Spec
  ( spec,
  )
where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Test.Hspec (Spec, describe, it, shouldBe)
import Utils
import Year2021.Day9

spec :: Spec
spec =
  describe "solutions" $ do
    it "Part 1" $ do
      let raw =
            [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
              [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
              [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
              [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
              [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
            ]
          exampleGrid = toGrid raw
      Map.elems (Map.filterWithKey (isLowPoint exampleGrid) exampleGrid) `shouldBe` [1, 5, 5, 0]
      answer1 <- solution1
      answer1 `shouldBe` 417
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 1148965
