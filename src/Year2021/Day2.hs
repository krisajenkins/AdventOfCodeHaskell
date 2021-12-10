{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Year2021.Day2
  ( solution1,
    solution2,
  )
where

import Control.Lens (over, view, _1, _2)
import Control.Lens.Operators ((#))
import Control.Lens.TH (makeLenses)
import Text.Megaparsec (sepEndBy, (<|>))
import Text.Megaparsec.Char (eol, space, string)
import Utils (Parser, integer, simpleParse)

data Position = Position
  { _depth :: Int,
    _x :: Int,
    _aim :: Int
  }
  deriving (Show, Eq)

makeLenses ''Position

initialPosition :: Position
initialPosition =
  Position
    { _depth = 0,
      _x = 0,
      _aim = 0
    }

data Move = Up Int | Down Int | Forward Int
  deriving (Show, Eq)

parseMove :: Parser m Move
parseMove =
  (Forward <$> (string "forward" *> space *> integer))
    <|> (Up <$> (string "up" *> space *> integer))
    <|> (Down <$> (string "down" *> space *> integer))

readMoves :: IO [Move]
readMoves = simpleParse "data/Year2021/Day2.txt" (parseMove `sepEndBy` eol)

handleSimpleMove :: Position -> Move -> Position
handleSimpleMove p (Up n) = over depth (\d -> d - n) p
handleSimpleMove p (Down n) = over depth (+ n) p
handleSimpleMove p (Forward n) = over x (+ n) p

solution1 :: IO Int
solution1 = do
  moves <- readMoves
  let finalPosition :: Position
      finalPosition = foldl handleSimpleMove initialPosition moves
  print finalPosition
  pure (view x finalPosition * view depth finalPosition)

handleComplexMove :: Position -> Move -> Position
handleComplexMove p (Up n) = over aim (\a -> a - n) p
handleComplexMove p (Down n) = over aim (+ n) p
handleComplexMove p (Forward n) =
  (\newP -> over depth (+ (view aim newP * n)) newP) $
    over x (+ n) p

solution2 :: IO Int
solution2 = do
  moves <- readMoves
  let finalPosition :: Position
      finalPosition = foldl handleComplexMove initialPosition moves
  print finalPosition
  pure (view x finalPosition * view depth finalPosition)
