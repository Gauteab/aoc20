{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import qualified Data.IntMap as IntMap
import Relude

main :: IO ()
main = do
  let Just input = nonEmpty [15, 12, 0, 14, 3, 1]
  print $ part2 input 2020 -- 249
  print $ part2 input 30000000

-- part 2:
-- ➜  aoc20 git:(main) ✗ time ./Day15
-- 41687
-- ./Day15  30.69s user 0.49s system 100% cpu 31.173 total

part2 :: NonEmpty Int -> Int -> Int
part2 input target = solve (length input - 1) (initialMap, last input)
  where
    initialMap = IntMap.fromList $ zip (toList $ init input) [0 ..]

    solve :: Int -> (IntMap Int, Int) -> Int
    solve i (_, current) | i == target - 1 = current
    solve i (seen, current) = solve (i + 1) $
      case seen ^. at current of
        Nothing -> (seen & at current ?~ i, 0)
        Just i' -> (seen & at current ?~ i, i - i')

-- inefficient solution:
-- part1 input target =
--   let initialMap = Map.fromList $ (zip (toList $ init input) [0 ..])
--       current = last input
--       l = length input
--       result = iterate step (initialMap, current, l - 1)
--    in result ^? ix (target - 1 - l) . _2

-- type Game = (Map Int Int, Int, Int)

-- step :: Game -> Game
-- step (seen, current, i) = case seen ^. at current of
--   Nothing -> (seen & at current ?~ i, 0, i + 1)
--   Just i' -> (seen & at current ?~ i, i - i', i + 1)
