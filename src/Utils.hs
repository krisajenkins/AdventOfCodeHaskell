{-# LANGUAGE LambdaCase #-}

module Utils where

import Data.Void (Void)
import Paths_adventofcode (getDataFileName)
import Text.Megaparsec (ParsecT, runParserT)
import Text.Megaparsec.Char.Lexer (decimal, lexeme, signed)

simpleParse :: FilePath -> ParsecT Void String IO a -> IO a
simpleParse datafile parser = do
  filename <- getDataFileName datafile
  contents <- readFile filename
  runParserT parser filename contents >>= \case
    Left err -> fail $ show err
    Right value -> pure value

integer :: ParsecT Void String IO Int
integer = signed (pure ()) (lexeme (pure ()) decimal)
