{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day8
  ( solution1,
    solution2,
    parseLine,
    solveLine,
    seqToInt,
    loebMap,
    Line (..),
    Wire (..),
  )
where

import Control.Monad (void)
import Control.Monad.Identity (join)
import Data.Functor (($>))
import Data.List (find)
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Data.Set (Set, union)
import qualified Data.Set as Set
import Text.Megaparsec (choice, optional, sepEndBy, some)
import Text.Megaparsec.Char (char, eol, spaceChar, string)
import Utils (Parser, flipMap, loeb, simpleParse)
import Prelude hiding (lines, lookup, map)

data Wire = A | B | C | D | E | F | G
  deriving (Show, Eq, Ord, Enum, Bounded)

data Segment = Top | Middle | Bottom | TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Show, Eq, Ord, Enum, Bounded)

parseWire :: Parser m Wire
parseWire =
  choice
    [ char 'a' $> A,
      char 'b' $> B,
      char 'c' $> C,
      char 'd' $> D,
      char 'e' $> E,
      char 'f' $> F,
      char 'g' $> G
    ]

parsePattern :: Parser m (Set Wire)
parsePattern = Set.fromList <$> some parseWire

data Line = Line
  { _inputs :: [Set Wire],
    _outputs :: [Set Wire]
  }
  deriving (Show, Eq)

parseLine :: Parser m Line
parseLine = do
  inputWires <- some (parsePattern <* spaceChar)
  void $ string "| "
  outputWires <- some (parsePattern <* optional (char ' '))
  pure $ Line inputWires outputWires

solveLine :: Line -> Maybe [Int]
solveLine line@Line {_inputs, _outputs} =
  traverse (`lookup` table) _outputs
  where
    table = flipMap $ Map.mapMaybe id $ createLookupTable line

readSequence :: IO [Line]
readSequence = simpleParse "data/Year2021/Day8.txt" (parseLine `sepEndBy` eol)

solution1 :: IO Int
solution1 = do
  outputs <- fmap _outputs <$> readSequence
  let matches = filter (\s -> length s `elem` [2, 3, 4, 7]) $ concat outputs
  pure $ length matches

solution2 :: IO (Maybe Int)
solution2 = do
  lines <- readSequence
  let answers :: [Maybe Int]
      answers = fmap seqToInt . solveLine <$> lines
  pure (sum <$> sequence answers)

seqToInt :: Num a => [a] -> a
seqToInt xs = sum $ zipWith (\x p -> x * (10 ^ p)) (reverse xs) [0 :: Integer ..]

-- | Same as 'loeb' with the type signature pinned down.
loebMap :: Map Int (Map Int a -> a) -> Map Int a
loebMap = loeb

createLookupTable :: Line -> Map Int (Maybe (Set Wire))
createLookupTable Line {_inputs, _outputs} =
  loeb $
    Map.fromList
      [ ( 0,
          \m -> do
            one <- join $ lookup 1 m
            three <- join $ lookup 3 m
            match
              ( \set ->
                  Set.size set == 6
                    && set `union` one == set
                    && Set.size (set `union` three) == 7
              )
        ),
        (1, \_ -> match (\set -> Set.size set == 2)),
        ( 2,
          \m -> do
            one <- join $ lookup 1 m
            nine <- join $ lookup 9 m
            match
              ( \set ->
                  Set.size set == 5
                    && set `union` one /= set
                    && set `union` one /= nine
              )
        ),
        ( 3,
          \m -> do
            one <- join $ lookup 1 m
            match
              ( \set ->
                  Set.size set == 5
                    && set `union` one == set
              )
        ),
        (4, \_ -> match (\set -> Set.size set == 4)),
        ( 5,
          \m -> do
            one <- join $ lookup 1 m
            nine <- join $ lookup 9 m
            match
              ( \set ->
                  Set.size set == 5
                    && set `union` one == nine
              )
        ),
        ( 6,
          \m -> do
            one <- join $ lookup 1 m
            three <- join $ lookup 1 m
            match
              ( \set ->
                  Set.size set == 6
                    && set `union` one /= set
                    && Set.size (set `union` three) == 7
              )
        ),
        (7, \_ -> match (\set -> Set.size set == 3)),
        (8, \_ -> match (\set -> Set.size set == 7)),
        ( 9,
          \m -> do
            three <- join $ lookup 3 m
            match
              ( \set ->
                  Set.size set == 6
                    && set `union` three == set
              )
        )
      ]
  where
    match p = find p _inputs
