module Year2018.Day5Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2018.Day5 (reactAll, solution1, solution2)

spec :: Spec
spec =
  describe "solutions" $ do
    it "reactAll" $ reactAll "abBA" `shouldBe` ""
    it "reactAll" $ reactAll "ABba" `shouldBe` ""
    it "reactAll" $ reactAll "aabAAB" `shouldBe` "aabAAB"
    it "sample" $ length (reactAll "dabAcCaCBAcCcaDA") `shouldBe` 10
    it "solution1 " $ solution1 >>= (`shouldBe` 10496)
    it "solution2 " $ solution2 >>= (`shouldBe` (5774, 'h'))
