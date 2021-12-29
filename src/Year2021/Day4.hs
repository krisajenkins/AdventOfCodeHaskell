{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Year2021.Day4
  ( solution1,
    solution2,
    calls,
    rawCards,
    Puzzle (..),
    readPuzzle,
    puzzleToCards,
    X (..),
    Y (..),
    CardId (..),
    Card (..),
    Value (..),
    Time (..),
    cardId,
    values,
  )
where

import Control.Lens (filtered, preview, set, toListOf, traversed, view, _1, _2)
import Control.Lens.At (at)
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import Data.Function (on)
import Data.Ix (Ix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Safe (maximumByMay, minimumMay)
import Safe.Foldable (minimumByMay)
import Text.Megaparsec (count, optional, sepBy, some)
import Text.Megaparsec.Char (char, eol, space)
import Utils (Parser, foldlWithIndex, integer, mapWithIndex, simpleParse)

newtype X = X Int deriving newtype (Show, Eq, Ord, Ix, Enum)

newtype Y = Y Int deriving newtype (Show, Eq, Ord, Ix, Enum)

newtype CardId = CardId Int deriving newtype (Show, Eq, Ord)

newtype Value = Value Int deriving newtype (Show, Eq, Ord, Num)

newtype Time = Time Int deriving newtype (Show, Eq, Ord, Enum)

type RawCard = [[Value]]

data Puzzle = Puzzle
  { _calls :: [Value],
    _rawCards :: [RawCard]
  }
  deriving (Show, Eq)

makeLenses ''Puzzle

------------------------------------------------------------
parseValue :: Parser m Value
parseValue = Value <$> integer

parseBingoCalls :: Parser m [Value]
parseBingoCalls = (parseValue `sepBy` char ',') <* eol

parseBingoCardLine :: Parser m [Value]
parseBingoCardLine = count 5 (parseValue <* optional space)

parseBingoCard :: Parser m [[Value]]
parseBingoCard = count 5 parseBingoCardLine

parseData :: Parser m Puzzle
parseData = do
  _calls <- parseBingoCalls
  void eol
  _rawCards <- some parseBingoCard
  pure Puzzle {_calls, _rawCards}

readPuzzle :: IO Puzzle
readPuzzle = simpleParse "data/Year2021/Day4.txt" parseData

------------------------------------------------------------
data Card = Card
  { _cardId :: CardId,
    _values :: Map (X, Y) (Value, Maybe Time)
  }
  deriving (Show, Eq, Ord)

makeLenses ''Card

puzzleToCards :: Puzzle -> [Card]
puzzleToCards Puzzle {_calls, _rawCards} =
  mapWithIndex (rawsCardToCards . CardId) _rawCards
  where
    callTimes :: Map Value Time
    callTimes = Map.fromList $ zip _calls (Time <$> [0 ..])

    rawsCardToCards :: CardId -> RawCard -> Card
    rawsCardToCards ci = foldlWithIndex (rowToCard . Y) (Card {_cardId = ci, _values = Map.empty})

    rowToCard :: Y -> Card -> [Value] -> Card
    rowToCard y = foldlWithIndex (itemToCard y . X)

    itemToCard :: Y -> X -> Card -> Value -> Card
    itemToCard y x card value = set (values . at (x, y)) (Just (value, Map.lookup value callTimes)) card

------------------------------------------------------------

findFirstCompleteLine :: [Card] -> Maybe (Card, Time)
findFirstCompleteLine cards = minimumByMay (compare `on` snd) $ cardTimes cards

findLastCompleteLine :: [Card] -> Maybe (Card, Time)
findLastCompleteLine cards = maximumByMay (compare `on` snd) $ cardTimes cards

cardTimes :: [Card] -> [(Card, Time)]
cardTimes = mapMaybe (\card -> fmap (card,) (findLineTimes card))

findLineTimes :: Card -> Maybe Time
findLineTimes card =
  minimumMay (Map.elems (fst allTimes) <> Map.elems (snd allTimes))
  where
    allTimes :: (Map X Time, Map Y Time)
    allTimes = Map.foldrWithKey f (Map.empty, Map.empty) (view values card)
    f :: (X, Y) -> (Value, Maybe Time) -> (Map X Time, Map Y Time) -> (Map X Time, Map Y Time)
    f (x1, y1) (_, mTime) (maxX, maxY) =
      ( Map.alter (g mTime) x1 maxX,
        Map.alter (g mTime) y1 maxY
      )

    g :: Maybe Time -> Maybe Time -> Maybe Time
    g Nothing _ = Nothing
    g (Just t) Nothing = Just t
    g (Just t1) (Just t2) = Just (max t1 t2)

calculateCardScore :: (Card, Time) -> Maybe Value
calculateCardScore x = do
  winningNumber <- calculateWinningNumber x
  let uncalledNumbers :: [Value]
      uncalledNumbers = calculateUncalledNumbers x
  pure $ winningNumber * sum uncalledNumbers

calculateWinningNumber :: (Card, Time) -> Maybe Value
calculateWinningNumber (winningCard, winsAt) =
  preview (values . traversed . filtered (\(_, mT) -> mT == Just winsAt) . _1) winningCard

calculateUncalledNumbers :: (Card, Time) -> [Value]
calculateUncalledNumbers (winningCard, winsAt) =
  toListOf (values . traversed . filtered (\(_, mT) -> mT > Just winsAt) . _1) winningCard

------------------------------------------------------------
-- Strategy - Label each value on the card with the time that it gets ticked (or Nothing if it hasn't been).
-- With that, it's fairly easy to find the winning time and the card that ticks it.
solution1 :: IO (Maybe Value)
solution1 = do
  cards <- puzzleToCards <$> readPuzzle
  pure $ findFirstCompleteLine cards >>= calculateCardScore

solution2 :: IO (Maybe Value)
solution2 = do
  cards <- puzzleToCards <$> readPuzzle
  pure $ findLastCompleteLine cards >>= calculateCardScore
