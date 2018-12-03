module Year2018.Day2
  ( solution1
  , solution2
  ) where

import Control.Foldl (Fold)
import qualified Control.Foldl as L
import Data.Map (elems)
import Data.Monoid (Sum(Sum))
import Text.Megaparsec (many, sepEndBy)
import Text.Megaparsec.Char (eol, letterChar)
import Utils (frequency, simpleParse)

------------------------------------------------------------
readSequence :: IO [String]
readSequence =
  simpleParse "data/Year2018/Day2.txt" (many letterChar `sepEndBy` eol)

seen :: Eq a => a -> Fold a Bool
seen = L.any . (==)

solution1 :: IO Int
solution1 = do
  (Sum as, Sum bs) <-
    foldMap
      (let classify True = 1
           classify False = 0
           countIf = fmap (Sum . classify) . seen
           checksum = (,) <$> countIf 2 <*> countIf 3
        in L.fold checksum . elems . frequency) <$>
    readSequence
  pure $ as * bs

solution2 :: IO [(String, String)]
solution2 = do
  codes <- readSequence
  pure $ do
    x <- codes
    y <- filter (x <) codes
    let characterDifferences =
          foldMap $ \(a, b) ->
            if a == b
              then ([], [a])
              else ([a, b], [])
        match = characterDifferences $ zip x y
    case match of
      ([_, _], _) -> pure match
      _ -> mempty
