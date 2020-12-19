{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Data.List (span)
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
  let valid = Set.fromList $ compileRules rules 0
  print $ length . filter (`Set.member` valid) $ input
  let fortyTwo = Set.fromList $ Text.pack <$> compileRules rules 42
      thirtyOne = Set.fromList $ Text.pack <$> compileRules rules 31
      f xs =
        let xs' = Text.pack xs & Text.chunksOf 8 & reverse
            (ys, ys') = span (`Set.member` thirtyOne) xs'
         in not (null ys) && length ys' > length ys && all (`Set.member` fortyTwo) ys'
  print $ length . filter f $ input

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
        [ Left <$> many1 (try (char ' ' *> nat)) `sepBy1` try (string " |")
        , Right <$> (char ' ' *> target)
        ]
    target = char '"' *> letter <* char '"'
