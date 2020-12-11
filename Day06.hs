{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.List (foldr1)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude

main :: IO ()
main = do
  input <- map lines . Text.splitOn "\n\n" <$> readFileText "./input/day6"
  print $ part1 input -- 6291
  print $ part2 input -- 3052

part1 :: [[Text]] -> Int
part1 = sum . map (length . f)
  where
    f = foldMap (Set.fromList . toString)

part2 :: [[Text]] -> Int
part2 = sum . map (length . f)
  where
    f = foldr1 Set.intersection . map (Set.fromList . toString)
