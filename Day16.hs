{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens hiding (noneOf)
import Data.List.Extra (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Maybes (fromJust)
import Relude
import Text.Parsec
import Text.ParserCombinators.Parsec.Number (nat)

main :: IO ()
main = do
  Right input <- parse parser "" <$> readFile "./input/day16"
  print $ part1 input -- 21071
  print $ part2 input -- 3429967441937

data Input = Input (Map String [Condition]) Ticket [Ticket] deriving (Show)
type Ticket = [Int]
type Condition = (Int, Int)

parser = Input <$> conditions <*> myTicket <*> nearbyTickets
  where
    conditions = Map.fromList <$> condition `sepEndBy` newline <* newline
    condition = try $ (,) <$> many1 (noneOf ":") <* string ": " <*> (range `sepBy` string " or ")
    range = (,) <$> nat <* char '-' <*> nat
    myTicket = string "your ticket:" *> space *> ticket <* spaces
    nearbyTickets = string "nearby tickets:" *> newline *> (ticket `sepEndBy` newline)
    ticket = nat `sepBy1` char ','

part1 (Input conditions _ nearbyTickets) = sum . concatMap (filter isInvalid) $ nearbyTickets
  where
    isInvalid x = not $ any (uncurry (inRange x)) conditions'
    conditions' = concat (Map.elems conditions)

part2 (Input conditions myTicket nearbyTickets) =
  product . map fst . filter (isPrefixOf "departure" . snd) <$> itraverse (\i x -> (x,) <$> Map.lookup i table) myTicket
  where
    table = Map.fromList $ f (Map.toList candidates') & each . _2 %~ fromJust . viaNonEmpty head . Set.toList
    f cs = case partition ((== 1) . length . snd) cs of
      (xs, []) -> xs
      (xs, ys) -> xs <> f (removeCandidates xs ys)
    removeCandidates :: [(Int, Set String)] -> [(Int, Set String)] -> [(Int, Set String)]
    removeCandidates (Set.unions . map snd -> singletons) ys = map (fmap $ flip Set.difference singletons) ys
    candidates' = Map.fromListWith Set.intersection candidates
    indexedEntries = concat $ zip [0 ..] <$> validTickets
    candidates = indexedEntries & each . _2 %~ possibilities
    possibilities x = Set.fromList . Map.keys $ Map.filter (any $ uncurry (inRange x)) conditions
    validTickets = filter (not . any isInvalid) nearbyTickets
    isInvalid x = not $ any (uncurry (inRange x)) conditions'
    conditions' = concat (Map.elems conditions)

inRange :: Ord a => a -> a -> a -> Bool
inRange x l u = x >= l && x <= u
