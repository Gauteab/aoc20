{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import qualified Data.Set as Set
import qualified Data.Text as T
import Relude hiding ((<|>))
import Text.Parsec

type Parser = Parsec Text ()

main :: IO ()
main = do
    input <- fetch pair
    print $ length $ filter isValid input -- 204

    part2 <- fetch validPair
    print $ length $ filter isValid part2 -- 179

fetch :: Parser String -> IO [[String]]
fetch p = mapMaybe (rightToMaybe . parse (parser p) "")
            . T.splitOn "\n\n"
            <$> readFileText "./input/day4"

parser :: Parser String -> Parser [String]
parser p = p `sepBy` space

pair :: Parser String
pair = many1 letter <* char ':' <* many1 (noneOf " \n")

-- Part 1
isValid :: [String] -> Bool
isValid = Set.isSubsetOf required . Set.fromList
  where
    required = Set.fromList ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

key :: String -> Parser String
key s = try $ string s <* char ':'

validPair :: Parser String
validPair = choice
    [ key "byr" <* validRange 1920 2002
    , key "iyr" <* validRange 2010 2020
    , key "eyr" <* validRange 2020 2030
    , key "hgt" <* heightP
    , key "hcl" <* (char '#' >> replicateM 6 (digit <|> oneOf ['a' .. 'f']))
    , key "ecl" <* choice (try . string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    , key "pid" <* replicateM 9 digit
    , key "cid" <* many1 (noneOf " \n")
    ]

inRange :: Ord a => a -> a -> a -> Bool
inRange x l u = x >= l && x <= u

validRange :: Int -> Int -> Parser ()
validRange i j = do
  Just x <- readMaybe <$> many1 digit
  guard $ inRange x i j

parses :: Parsec String () a -> String -> Bool
parses p x = isRight $ parse (p <* eof) "" x

heightP :: Parser ()
heightP = do
    Just number <- readMaybe <$> many1 digit
    choice
      [ string "cm" *> guard (inRange number 150 193)
      , string "in" *> guard (inRange number 59 76) ]
