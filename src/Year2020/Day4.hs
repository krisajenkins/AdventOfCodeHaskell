{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day4 where

import Control.Monad (guard, void)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec ((<|>), choice, sepEndBy, some, try)
import Text.Megaparsec.Char
  ( alphaNumChar
  , char
  , digitChar
  , eol
  , hexDigitChar
  , letterChar
  , spaceChar
  , string
  )
import Utils (Parser, integer, simpleParse)

data ValidatedField
  = Valid String
  | Invalid String
  deriving (Show, Eq)

type Passport = Map String ValidatedField

fields :: Map String (Parser m String)
fields =
  Map.fromList
    [ ( "byr"
      , do v <- integer
           guard (1920 <= v && v <= 2002)
           pure $ show v)
    , ( "iyr"
      , do v <- integer
           guard (2010 <= v && v <= 2020)
           pure $ show v)
    , ( "eyr"
      , do v <- integer
           guard (2020 <= v && v <= 2030)
           pure $ show v)
    , ( "hgt"
      , try
          (do v <- integer
              units <- string "cm"
              guard (150 <= v && v <= 193)
              pure $ show v <> units) <|>
        (do v <- integer
            units <- string "in"
            guard (59 <= v && v <= 76)
            pure $ show v <> units))
    , ( "hcl"
      , do void $ char '#'
           hex <- some hexDigitChar
           guard (length hex == 6)
           pure $ '#' : hex)
    , ( "ecl"
      , choice (string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))
    , ( "pid"
      , do pid <- some digitChar
           guard (length pid == 9)
           pure pid)
    , ("cid", fieldChars)
    ]

fieldChars :: Parser m String
fieldChars = some (choice [alphaNumChar, char '#'])

parsePassport :: Parser m Passport
parsePassport =
  Map.fromList <$> (fieldValidator `sepEndBy` choice [void spaceChar, void eol])

fieldValidator :: Parser m (String, ValidatedField)
fieldValidator =
  try (choice (fmap fieldParser (Map.toList fields))) <|> badParser

fieldParser :: (String, Parser m String) -> Parser m (String, ValidatedField)
fieldParser (code, parser) = do
  void $ string code
  void $ char ':'
  value <- parser
  pure (code, Valid value)

badParser :: Parser m (String, ValidatedField)
badParser = do
  code <- some letterChar
  void $ char ':'
  value <- fieldChars
  pure (code, Invalid value)

parsePassports :: Parser m [Passport]
parsePassports = parsePassport `sepEndBy` eol

readSequence :: IO [Passport]
readSequence = simpleParse "data/Year2020/Day4.txt" parsePassports

isValidSimple :: Passport -> Bool
isValidSimple m = ignoreCid (Map.keysSet fields) == ignoreCid (Map.keysSet m)
  where
    ignoreCid = Set.delete "cid"

solution1 :: IO Int
solution1 = length . filter isValidSimple <$> readSequence

isValid :: Passport -> Bool
isValid m =
  isValidSimple m &&
  all
    (\case
       Valid _ -> True
       Invalid _ -> False)
    (Map.elems m)

solution2 :: IO Int
solution2 = length . filter isValid <$> readSequence
