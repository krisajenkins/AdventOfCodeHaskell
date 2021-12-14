{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day3
  ( solution1,
    solution2,
  )
where

import Control.Applicative (some)
import Data.List.Extra (sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ord (Down (Down))
import Safe (atMay, headMay)
import Text.Megaparsec (ParseErrorBundle, runParser, sepEndBy)
import Text.Megaparsec.Char (eol, numberChar)
import Text.Megaparsec.Char.Lexer (binary)
import Utils (frequency, simpleParse)

readSequence :: IO [String]
readSequence = simpleParse "data/Year2021/Day3.txt" (some numberChar `sepEndBy` eol)

solution1 :: IO (Maybe Integer)
solution1 = do
  bytes <- readSequence
  let gammaString :: String
      gammaString = popularStringByIndex mostCommonByIndex 0 bytes []
  let epsilonString :: String
      epsilonString = popularStringByIndex leastCommonByIndex 0 bytes []
  let gamma = parseBinary gammaString
  let epsilon = parseBinary epsilonString
  pure ((*) <$> gamma <*> epsilon)

mostCommonByIndex :: Ord a => Int -> [[a]] -> Maybe a
mostCommonByIndex =
  _commonByIndex (\(char, count) -> (Down count, Down char))

leastCommonByIndex :: Ord a => Int -> [[a]] -> Maybe a
leastCommonByIndex =
  _commonByIndex (\(char, count) -> (count, char))

_commonByIndex :: Ord a => Ord b => ((a, Int) -> b) -> Int -> [[a]] -> Maybe a
_commonByIndex sorter index =
  fmap fst
    . headMay
    . sortOn sorter
    . Map.toList
    . frequency
    . mapMaybe (`atMay` index)

popularStringByIndex :: (Int -> [String] -> Maybe Char) -> Int -> [String] -> [Maybe Char] -> String
popularStringByIndex cmp index strs acc
  | index > maximum (fmap length strs) = reverse $ catMaybes acc
  | otherwise =
    popularStringByIndex
      cmp
      (index + 1)
      (filter (not . null) strs)
      (cmp index strs : acc)

popularStringByWhittling :: (Int -> [String] -> Maybe Char) -> Int -> [String] -> Maybe String
popularStringByWhittling cmp index strs
  | length strs <= 1 = headMay strs
  | otherwise =
    let target = cmp index strs
     in popularStringByWhittling
          cmp
          (index + 1)
          (filter (\s -> s `atMay` index == target) strs)

solution2 :: IO (Maybe Integer)
solution2 = do
  bytes <- readSequence
  let oxygenString = fromMaybe "" $ popularStringByWhittling mostCommonByIndex 0 bytes
  let co2String = fromMaybe "" $ popularStringByWhittling leastCommonByIndex 0 bytes
  let oxygen = parseBinary oxygenString
  let co2 = parseBinary co2String
  pure ((*) <$> oxygen <*> co2)

parseBinary :: String -> Maybe Integer
parseBinary = hush . (runParser binary "" :: String -> Either (ParseErrorBundle String String) Integer)

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a
