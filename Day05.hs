{-# LANGUAGE NoImplicitPrelude #-}

import Data.Foldable
import qualified Data.Set as Set
import Relude

part1 :: [Char] -> Integer
part1 input = row * 8 + col
  where
    (fb, lr) = splitAt 7 input
    row = fst $ foldl' nextBound (0, 127) fb
    col = fst $ foldl' nextBound (0, 7) lr

part2 :: [Integer] -> Set Integer
part2 input =
  Set.fromList [minimum input .. maximum input] `Set.difference` Set.fromList input

nextBound :: (Integer, Integer) -> Char -> (Integer, Integer)
nextBound (lower, upper) c =
  let newBound = fromInteger (lower + upper) / 2
   in case c of
        'B' -> (ceiling newBound, upper)
        'R' -> (ceiling newBound, upper)
        'F' -> (lower, floor newBound)
        'L' -> (lower, floor newBound)

main :: IO ()
main = do
  input <- map toString . lines <$> readFileText "./input/day5"
  print $ maximum (part1 <$> input) -- 959
  print $ part2 (part1 <$> input) -- 527
