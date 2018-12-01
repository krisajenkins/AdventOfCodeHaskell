module Year2018.Day1Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2018.Day1 (solution1, solution2)

spec :: Spec
spec =
  describe "solutions" $ do
    it "solution1 " $ solution1 >>= (`shouldBe` 536)
    it "solution2 " $ solution2 >>= (`shouldBe` 75108)
