{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day6
  ( solution1
  , solution2
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (eol, letterChar)
import Utils (Parser, simpleParse)

type Group = [String]

parseGroup :: Parser m [String]
parseGroup = some letterChar `sepEndBy` eol

readSequence :: IO [Group]
readSequence = simpleParse "data/Year2020/Day6.txt" (parseGroup `sepEndBy` eol)

countGroup :: Group -> Int
countGroup = Set.size . Set.fromList . mconcat

solution1 :: IO Int
solution1 = sum . fmap countGroup <$> readSequence

tallyGroup :: Group -> Int
tallyGroup =
  Set.size . fromMaybe Set.empty . foldl f Nothing . fmap Set.fromList
  where
    f Nothing = Just
    f (Just xs) = Just . Set.intersection xs

solution2 :: IO Int
solution2 = sum . fmap tallyGroup <$> readSequence
