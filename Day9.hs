{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Control.Monad (foldM)
import Data.Foldable
import Day1 (twoSum)
import Relude

main :: IO ()
main = do
  Just input <- traverse (readMaybe @Int . toString) . lines <$> readFileText "./input/day9"
  let Left result = part1 input
  print result -- 27911108
  print $ part2 result input -- 4023754

part1 :: [Int] -> Either Int [Int]
part1 (splitAt 25 -> (preamble, input)) = foldM f preamble input
  where
    f candidates value | isJust $ twoSum value candidates = Right (drop 1 candidates <> [value])
    f _ value = Left value

part2 :: Int -> [Int] -> Maybe Int
part2 result = viaNonEmpty head . mapMaybe (fmap sumMinMax . subsequenceSummingTo result) . tails . takeWhile (/= result)

sumMinMax :: (Num a, Foldable t, Ord a) => t a -> a
sumMinMax xs = minimum xs + maximum xs

subsequenceSummingTo :: (Foldable t, Eq a, Num a) => a -> t a -> Maybe [a]
subsequenceSummingTo target = either Just (const Nothing) . foldM f (0, [])
  where
    f (s, xs) x | s + x == target = Left (x : xs)
    f (s, xs) x = Right (s + x, x : xs)
