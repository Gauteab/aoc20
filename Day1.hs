{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import qualified Data.Set as Set
import Relude

twoSum :: Int -> [Int] -> Maybe (Int, Int)
twoSum target = leftToMaybe . foldlM f Set.empty
  where
    f seen x | Set.member x seen = Left (x, target - x)
    f seen x = Right $ Set.insert (target - x) seen

threeSums :: Int -> [Int] -> [[Int]]
threeSums target numbers = do
  x <- numbers
  (y, z) <- maybeToList (twoSum (target - x) numbers)
  pure [x, y, z]

part1, part2 :: [Int] -> Maybe Int
part1 = fmap (uncurry (*)) . twoSum 2020
part2 = viaNonEmpty head . map product . threeSums 2020

main :: IO ()
main = do
  Just input <- traverse (readMaybe @Int . toString) . lines <$> readFileText "./day1.in"
  print $ part1 input -- 858496
  print $ part2 input -- 263819430
