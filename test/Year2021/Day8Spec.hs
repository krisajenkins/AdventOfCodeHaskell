{-# LANGUAGE OverloadedLists #-}

module Year2021.Day8Spec
  ( spec,
  )
where

import Control.Lens (_Void)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (ParseErrorBundle (ParseErrorBundle), runParser, runParserT)
import Year2021.Day8

spec :: Spec
spec =
  describe "solutions" $ do
    it "Example Solve" $ do
      let exampleString = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
      let example :: Either (ParseErrorBundle String Void) Line
          example = runParser parseLine "<str>" exampleString
      example
        `shouldBe` Right
          ( Line
              [ [A, C, E, D, G, F, B],
                [C, D, F, B, E],
                [G, C, D, F, A],
                [F, B, C, A, D],
                [D, A, B],
                [C, E, F, A, B, D],
                [C, D, F, G, E, B],
                [E, A, F, B],
                [C, A, G, E, D, B],
                [A, B]
              ]
              [ [C, D, F, E, B],
                [F, C, A, D, B],
                [C, D, F, E, B],
                [C, D, B, A, F]
              ]
          )
      solveLine <$> example `shouldBe` Right (Just [5, 3, 5, 3])
      let exampleLines :: [String]
          exampleLines =
            [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
              "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
              "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
              "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
              "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
              "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
              "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
              "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
              "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
              "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
              "gfeab aefdgc cdefgb bcfgd ad fad adbc acbfgd fgdba bgfedac | fgcdeba da gfdab cegdaf",
              "dg fadgceb dacbef agfeb gcdbef edcbf gdf ecgd cgbadf defbg | bedcf bgdfac cbfedg abfeg",
              "ecf fc gdefc feabdg dcabef edagc bfdge fgdbec fcbaedg fbcg | efc bgfcde caedg gdfce"
            ]
          exampleSolutions = fmap (fmap (fmap seqToInt . solveLine) . runParser parseLine "<str>") exampleLines
      exampleSolutions
        `shouldBe` [ Right (Just 8394),
                     Right (Just 9781),
                     Right (Just 1197),
                     Right (Just 9361),
                     Right (Just 4873),
                     Right (Just 8418),
                     Right (Just 4548),
                     Right (Just 1625),
                     Right (Just 8717),
                     Right (Just 4315),
                     Right (Just 8130),
                     Right (Just 5092),
                     Right (Just 7923)
                   ]
    it
      "Part 1"
      $ do
        answer1 <- solution1
        answer1 `shouldBe` 349

    it
      "Part 2"
      $ do
        answer2 <- solution2
        answer2 `shouldBe` Just 1070957
