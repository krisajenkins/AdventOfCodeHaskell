{-# LANGUAGE OverloadedLists #-}

module Year2018.Day8Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2018.Day8 (Tree(Node), buildTree, solution1, solution2, value)

exampleInput :: [Int]
exampleInput = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]

spec :: Spec
spec =
  describe "solutions" $ do
    it "buildTree simple" $
      buildTree [0, 3, 1, 2, 3] `shouldBe` Node [1, 2, 3] []
    it "buildTree nested" $
      buildTree [1, 3, 0, 0, 1, 2, 3] `shouldBe` Node [1, 2, 3] [Node [] []]
    it "buildTree example" $
      buildTree exampleInput `shouldBe`
      Node [1, 1, 2] [Node [10, 11, 12] [], Node [2] [Node [99] []]]
    it "solution1 " $ solution1 >>= (`shouldBe` 37262)
    it "value B" $ value (Node [10, 11, 12] []) `shouldBe` 33
    it "value C" $ value (Node [2] [Node [99] []]) `shouldBe` 0
    it "value D" $ value (Node [99] []) `shouldBe` 99
    it "value example" $ value (buildTree exampleInput) `shouldBe` 66
    it "value example" $ value (buildTree exampleInput) `shouldBe` 66
    it "solution2 " $ solution2 >>= (`shouldBe` 20839)
