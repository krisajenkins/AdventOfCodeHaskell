module Year2018.Day3Spec
  ( spec
  ) where

import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)
import Year2018.Day3 (PatchId(PatchId), solution1, solution2)

spec :: Spec
spec =
  describe "solutions" $ do
    it "solution1 " $ solution1 >>= (`shouldBe` 118223)
    it "solution2 " $ solution2 >>= (`shouldBe` (Set.singleton (PatchId 412)))
