{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day5
  ( solution1
  , reactAll
  , solution2
  ) where

import Data.Char (isLower, toUpper)
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Functor (($>))
import Data.List (sort, sortOn)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Void (Void)
import Text.Megaparsec (ParsecT, (<|>), between, many, manyTill, sepEndBy)
import Text.Megaparsec.Char (anyChar, char, eol, notChar, space, string)
import Utils (integer, simpleParse)

readSequence :: IO String
readSequence = simpleParse "data/Year2018/Day5.txt" (manyTill anyChar eol)

react' :: (String -> String) -> String -> String
react' _ [] = []
react' _ [x] = [x]
react' f (x:y:rest)
  | isLower x && toUpper x == y = f rest
  | isLower y && toUpper y == x = f rest
  | otherwise = x : f (y : rest)

react :: String -> String
react = fix react'

reactAll' :: (String -> String) -> String -> String
reactAll' f str =
  let one = react str
   in if one == str
        then str
        else f one

reactAll :: String -> String
reactAll = fix reactAll'

solution1 :: IO Int
solution1 = length . reactAll <$> readSequence

solution2 :: IO (Int, Char)
solution2 = do
  str <- readSequence
  let stats = do
        toRemove <- ['a' .. 'z']
        let newStr =
              filter ((&&) <$> (/= toRemove) <*> (/= toUpper toRemove)) str
            reacted = reactAll newStr
        pure (length reacted, toRemove)
  pure $ head $ sortOn fst stats
