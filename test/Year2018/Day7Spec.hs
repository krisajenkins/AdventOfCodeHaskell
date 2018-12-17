{-# LANGUAGE OverloadedLists #-}

module Year2018.Day7Spec
  ( spec
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import GraphUtils (toGraph)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotSatisfy)
import Year2018.Day7 (assemble, costOf, interpreter, solution1, solution2)

spec :: Spec
spec =
  describe "solutions" $ do
    it "assemble" $
      assemble
        (toGraph
           ([ ('C', 'A')
            , ('C', 'F')
            , ('A', 'B')
            , ('A', 'D')
            , ('B', 'E')
            , ('D', 'E')
            , ('F', 'E')
            ] :: [(Char, Char)])) `shouldBe`
      "CABDFE"
    it "assemble" $
      assemble
        (toGraph
           ([ ('A', 'B')
            , ('A', 'C')
            , ('A', 'D')
            , ('C', 'E')
            , ('B', 'E')
            , ('D', 'E')
            ] :: [(Char, Char)])) `shouldBe`
      "ABCDE"
    it "solution1 " $ solution1 >>= (`shouldBe` "JNOIKSYABEQRUVWXGTZFDMHLPC")
    it "costOf " $ costOf <$> "ABC" `shouldBe` [61, 62, 63]
    -- it "costOf " $ costOf <$> "ABC" `shouldBe` [1, 2, 3]
    -- it "interpreter" $
    --   maximum
    --     (fst <$>
    --      interpreter
    --        (toGraph
    --           ([ ('C', 'A')
    --            , ('C', 'F')
    --            , ('A', 'B')
    --            , ('A', 'D')
    --            , ('B', 'E')
    --            , ('D', 'E')
    --            , ('F', 'E')
    --            ] :: [(Char, Char)]))) `shouldBe`
    --   15
    it "solution2 " $
      solution2 >>=
      (`shouldNotSatisfy` flip
                            Set.member
                            [1848, 960, 1067, 1068, 1305, 849, 1319])
    it "solution2 " $ solution2 >>= (`shouldBe` 1099)
