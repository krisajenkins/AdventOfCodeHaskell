{-# LANGUAGE OverloadedLists #-}

module Year2020.Day4Spec
  ( spec
  ) where

import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Text.Megaparsec (parse)
import           Year2020.Day4

spec :: Spec
spec =
  describe "solutions" $ do
    it "Parses and validates individual fields" $ do
      let parsesTo input output =
            parse fieldValidator "<str>" input `shouldBe` Right output
      "byr:2002" `parsesTo` ("byr", Valid "2002")
      "byr:2003" `parsesTo` ("byr", Invalid "2003")
      "hgt:60in" `parsesTo` ("hgt", Valid "60in")
      "hgt:190cm" `parsesTo` ("hgt", Valid "190cm")
      "hgt:190in" `parsesTo` ("hgt", Invalid "190in")
      "hgt:190" `parsesTo` ("hgt", Invalid "190")
    it "Parses a single passport" $
      parse
        parsePassport
        "<str>"
        (unlines
           [ "eyr:2021 hgt:168cm hcl:#fffffd"
           , "pid:180778832"
           , "byr:1923 ecl:amb iyr:2019 cid:241"
           ]) `shouldBe`
      Right
        [ ("eyr", Valid "2021")
        , ("hgt", Valid "168cm")
        , ("hcl", Valid "#fffffd")
        , ("pid", Valid "180778832")
        , ("byr", Valid "1923")
        , ("ecl", Valid "amb")
        , ("iyr", Valid "2019")
        , ("cid", Valid "241")
        ]
    it "Parses multiple passports" $
      parse
        parsePassports
        "<str>"
        (unlines
           [ "eyr:2021 hgt:168cm hcl:#fffffd pid:180778832"
           , "byr:2013 ecl:amb iyr:2019 cid:241"
           , ""
           , "eyr:2021 hgt:200cm hcl:#fffffd pid:180778832"
           , "byr:1923 ecl:yel iyr:2019 cid:241"
           ]) `shouldBe`
      Right
        [ [ ("eyr", Valid "2021")
          , ("hgt", Valid "168cm")
          , ("hcl", Valid "#fffffd")
          , ("pid", Valid "180778832")
          , ("byr", Invalid "2013")
          , ("ecl", Valid "amb")
          , ("iyr", Valid "2019")
          , ("cid", Valid "241")
          ]
        , [ ("eyr", Valid "2021")
          , ("hgt", Invalid "200cm")
          , ("hcl", Valid "#fffffd")
          , ("pid", Valid "180778832")
          , ("byr", Valid "1923")
          , ("ecl", Invalid "yel")
          , ("iyr", Valid "2019")
          , ("cid", Valid "241")
          ]
        ]
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 260
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 153
