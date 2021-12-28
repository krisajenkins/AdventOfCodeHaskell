{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day10
  ( solution1,
    solution2,
    findCorruption,
    findCompletion,
    scoreStack,
    Stack (..),
  )
where

import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Data.Maybe
import Text.Megaparsec (oneOf, sepEndBy, some)
import Text.Megaparsec.Char (eol)
import Utils (Parser, simpleParse)
import Prelude hiding (lookup)

scores :: Map Char Integer
scores =
  Map.fromList
    [ (')', 3),
      (']', 57),
      ('}', 1197),
      ('>', 25137)
    ]

parseDelimiter :: Parser m Char
parseDelimiter =
  oneOf "[](){}<>"

newtype Stack = Stack String
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid)

isOpener :: Char -> Bool
isOpener '{' = True
isOpener '[' = True
isOpener '(' = True
isOpener '<' = True
isOpener _ = False

pairOf :: Char -> Char
pairOf '{' = '}'
pairOf '[' = ']'
pairOf '(' = ')'
pairOf '<' = '>'
pairOf x = x

findCorruption :: Stack -> String -> Maybe Char
findCorruption _ [] = Nothing
findCorruption (Stack []) [y] = Just y
findCorruption (Stack []) (y : ys)
  | isOpener y = findCorruption (Stack [y]) ys
findCorruption (Stack (x : xs)) (y : ys)
  | isOpener y = findCorruption (Stack (y : x : xs)) ys
  | y == pairOf x = findCorruption (Stack xs) ys
  | otherwise = Just y

findCompletion :: Stack -> String -> Maybe Stack
findCompletion (Stack stack) [] = Just (Stack (pairOf <$> stack))
findCompletion (Stack []) [y] = Nothing
findCompletion (Stack []) (y : ys)
  | isOpener y = findCompletion (Stack [y]) ys
findCompletion (Stack (x : xs)) (y : ys)
  | isOpener y = findCompletion (Stack (y : x : xs)) ys
  | y == pairOf x = findCompletion (Stack xs) ys
  | otherwise = Nothing

isCorrupt :: String -> Bool
isCorrupt = isJust . findCorruption mempty

readSequence :: IO [String]
readSequence = simpleParse "data/Year2021/Day10.txt" (some parseDelimiter `sepEndBy` eol)

solution1 :: IO Integer
solution1 = do
  strings <- readSequence
  let corruptions = mapMaybe (findCorruption mempty) strings
  pure $ sum $ mapMaybe (`lookup` scores) corruptions

scoreStack :: Stack -> Integer
scoreStack (Stack stack) =
  foldl
    ( \total c ->
        ( case c of
            ')' -> 1
            ']' -> 2
            '}' -> 3
            '>' -> 4
            _ -> 0
        )
          + 5 * total
    )
    0
    stack

solution2 :: IO Integer
solution2 = do
  strings <- readSequence
  let incompletes = filter (not . isCorrupt) strings
  let totals = sort $ scoreStack <$> mapMaybe (findCompletion mempty) incompletes
  let winningIndex = length totals `div` 2
  pure $ totals !! winningIndex
