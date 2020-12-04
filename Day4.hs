{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import qualified Data.Set as Set
import Relude hiding ((<|>))
import Text.Parsec

main :: IO ()
main = do
  Right input <- parse parser "" <$> readFileText "./input/day4"
  print $ length $ filter isValid input -- 204
  print $ length $ filter isValid' input -- 179

parser :: Parsec Text () [[(String, String)]]
parser = pairs `sepBy` space
  where
    pairs = many1 $ pair <* space
    pair = (,) <$> many1 letter <* char ':' <*> many1 (noneOf [' ', '\n'])

-- Part 1
isValid :: [(String, String)] -> Bool
isValid = Set.isSubsetOf required . Set.fromList . map fst
  where
    required = Set.fromList ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

-- Part 2
isValid' :: [(String, String)] -> Bool
isValid' xs = isValid xs && all validPair xs

validPair :: (String, String) -> Bool
validPair = \case
  ("byr", readMaybe -> Just birthYear) -> inRange birthYear 1920 2002
  ("iyr", readMaybe -> Just issueYear) -> inRange issueYear 2010 2020
  ("eyr", readMaybe -> Just expirationYear) -> inRange expirationYear 2020 2030
  ("hgt", parse heightP "" -> Right (Left (Just n))) -> inRange n 150 193
  ("hgt", parse heightP "" -> Right (Right (Just n))) -> inRange n 59 76
  ("hcl", x) -> parses (char '#' <* replicateM 6 (digit <|> oneOf ['a' .. 'f'])) x
  ("ecl", x) -> parses (choice $ try . string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) x
  ("pid", x) -> parses (replicateM 9 digit) x
  ("cid", _) -> True
  _ -> False

inRange :: Ord a => a -> a -> a -> Bool
inRange x l u = x >= l && x <= u

parses :: Parsec String () a -> String -> Bool
parses p x = isRight $ parse (p <* eof) "" x

heightP :: Parsec String () (Either (Maybe Integer) (Maybe Integer))
heightP = do
  number <- readMaybe <$> many1 digit
  string "cm" $> Left number <|> string "in" $> Right number
