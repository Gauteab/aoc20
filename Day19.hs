{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Control.Monad.Error (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (nat)

main :: IO ()
main = do
  Right (rules, input) <- parse parser "" <$> readFile "./input/day19"
  print $ input
  let valid = Set.fromList $ compileRules rules 0
  print $ length . filter (`Set.member` valid) $ input
  print $ part1 input --
  print $ part2 input --

compileRules :: Rules -> Integer -> [String]
compileRules rules i = case rules Map.! i of
  Right c -> [[c]]
  Left alternatives -> do
    xs <- alternatives
    x <- sequence (compileRules rules <$> xs)
    pure (concat x)

type Rules = Map Integer (Either [[Integer]] Char)
parser :: Parser (Rules, [[Char]])
parser = (,) <$> rules <* newline <*> inputs
  where
    inputs = many1 letter `sepEndBy` newline
    rules = Map.fromList <$> sepEndBy rule newline
    rule = (,) <$> nat <* try (string ":") <*> rhs
    rhs =
      choice
        [ Left <$> (many1 $ try (char ' ' *> nat)) `sepBy1` try (string " |")
        , Right <$> (char ' ' *> target)
        ]
    target = char '"' *> letter <* char '"'

part1 input = "TODO"

part2 input = "TODO"
