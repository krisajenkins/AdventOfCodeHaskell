{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Year2021.Day4Spec
  ( spec,
  )
where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Safe (atMay)
import Test.Hspec (Spec, describe, it, shouldBe)
import Year2021.Day4

spec :: Spec
spec =
  describe "solutions" $ do
    it "readPuzzle" $ do
      initialPuzzle <- readPuzzle
      length (view calls initialPuzzle) `shouldBe` 100
      length (view rawCards initialPuzzle) `shouldBe` 100
    it "puzzleToCards" $ do
      cards :: [Card] <- puzzleToCards <$> readPuzzle
      preview (ix 1 . cardId) cards `shouldBe` Just (CardId 1)
      preview (ix 1 . values . ix (X 0, Y 1)) cards `shouldBe` Just (Value 83, Just (Time 41))
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` Just (Value 10374)
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` Just (Value 24742)
