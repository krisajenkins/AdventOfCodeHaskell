{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day3 where

import Data.Functor (($>))
import Text.Megaparsec ((<|>), sepEndBy, some)
import Text.Megaparsec.Char (char, eol)
import Utils (Parser, simpleParse)

data Land
  = Tree
  | Ground
  deriving (Show, Eq)

type Row = [Land]

type Map = [Row]

parseLand :: Parser m Land
parseLand = (char '.' $> Ground) <|> (char '#' $> Tree)

parseRow :: Parser m Row
parseRow = some parseLand

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

ski :: Int -> Map -> Map
ski skip xs = foldl go [] (zip [0 ..] xs)
  where
    go :: Map -> (Int, Row) -> Map
    go ys (n, x) = ys <> [rotate (n * skip) x]

readSequence :: IO Map
readSequence = simpleParse "data/Year2020/Day3.txt" (parseRow `sepEndBy` eol)

showLand :: Land -> Char
showLand Ground = '.'
showLand Tree = '#'

countTrees :: Int -> Map -> Int
countTrees skipX puzzle =
  let skiid = ski skipX puzzle
      line = map head skiid
      trees = filter (Tree ==) line
   in length trees

solution1 :: IO Int
solution1 = countTrees 3 <$> readSequence

solution2 :: IO Int
solution2 = do
  puzzle <- readSequence
  pure $
    product
      [ countTrees 1 puzzle
      , countTrees 3 puzzle
      , countTrees 5 puzzle
      , countTrees 7 puzzle
      , countTrees 1 (evenRows puzzle)
      ]

evenRows :: [a] -> [a]
evenRows = map snd . filter (even . fst) . zip ([0 ..] :: [Int])
