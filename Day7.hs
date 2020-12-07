{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Relude hiding (optional)
import Text.Parsec

main :: IO ()
main = do
  Right input <- traverse (parse parser "") . lines <$> readFileText "./input/day7"
  print $ part1 input -- 208
  print $ part2 input -- 1664

type Entry = (String, [(Int, String)])

parser :: Parsec Text () Entry
parser = (,) <$> bag <* string " bags contain " <*> declaration `sepBy` string ", "
  where
    bag = (<>) <$> word <* space <*> word
    declaration = (,) <$> int <* space <*> bag <* string " bag" <* optional (char 's')
    int = fromJust . readMaybe <$> many1 digit
    word = many1 letter

hasBag :: String -> Entry -> Bool
hasBag s (_, bags) = s `elem` map snd bags

part1 :: [Entry] -> Int
part1 input = length . nub . concatMap (map fst) . takeWhile (not . null) $ iterate f initial
  where
    initial = filter (hasBag "shinygold") input
    f = concatMap $ \(name, _) -> filter (hasBag name) input

part2 :: [Entry] -> Maybe Int
part2 (Map.fromList -> input) = subtract 1 <$> go "shinygold"
  where
    go :: String -> Maybe Int
    go bag = do
      xs <- Map.lookup bag input
      (+ 1) . sum <$> traverse (\(n, name) -> (* n) <$> go name) xs
