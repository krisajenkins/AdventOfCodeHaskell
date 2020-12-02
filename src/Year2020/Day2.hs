{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day2 where

import           Control.Monad        (void)
import           Data.Void            (Void)
import           Safe                 (at)
import           Text.Megaparsec      (ParsecT, eof, manyTill)
import           Text.Megaparsec.Char (anyChar, char, eol, letterChar, space,
                                       string)
import           Utils                (integer, simpleParse)

data Entry =
  Entry
    { lowerBound :: Int
    , upperBound :: Int
    , check      :: Char
    , password   :: String
    }
  deriving (Show, Eq)

parseEntry :: ParsecT Void String m Entry
parseEntry = do
  lowerBound <- integer
  void $ char '-'
  upperBound <- integer
  space
  check <- letterChar
  void $ string ": "
  password <- manyTill anyChar eol
  pure Entry {..}

readSequence :: IO [Entry]
readSequence = simpleParse "data/Year2020/Day2.txt" (manyTill parseEntry eof)

isValidFrequency :: Entry -> Bool
isValidFrequency Entry {..} =
  let count = length $ filter (== check) password
   in lowerBound <= count && count <= upperBound

solution1 :: IO Int
solution1 = length . filter isValidFrequency <$> readSequence

isValidPosition :: Entry -> Bool
isValidPosition Entry {..} =
  test lowerBound `xor` test upperBound
  where
    test bound = check == (password `at` (bound - 1))

xor :: Bool -> Bool -> Bool
xor True True   = False
xor True False  = True
xor False True  = True
xor False False = False

solution2 :: IO Int
solution2 = length . filter isValidPosition <$> readSequence
