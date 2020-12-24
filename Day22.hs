{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude

p1 = [41, 26, 29, 11, 50, 38, 42, 20, 13, 9, 40, 43, 10, 24, 35, 30, 23, 15, 31, 48, 27, 44, 16, 12, 14]
p2 = [18, 6, 32, 37, 25, 21, 33, 28, 7, 8, 45, 46, 49, 5, 19, 2, 39, 4, 17, 3, 22, 1, 34, 36, 47]

-- p1 = [43, 19]
-- p2 = [2, 29, 14]

main :: IO ()
main = do
  -- print $ sort (p1 <> p2)
  -- print $ part1 p1 p2 -- 32489
  print $ part2 p1 p2

score = snd . foldr (\v (i, s) -> (i + 1, v * i + s)) (1, 0)

part1 p1 p2 = score $ combat p1 p2
part2 p1 p2 = recursiveCombat p1 p2 & both %~ score

combat [] p2 = p2
combat p1 [] = p1
combat (x : xs) (y : ys)
  | x > y = combat (xs <> [x, y]) ys
  | x < y = combat xs (ys <> [y, x])

recursiveCombat p1 p2 = combat Set.empty p1 p2
  where
    combat _ [] p2 = Right p2
    combat _ p1 [] = Left p1
    combat memory (p1@(x : xs)) p2@(y : ys)
      | Set.member (p1, p2) memory = Left p1
      | length xs >= x && length ys >= y =
        case recursiveCombat (take x xs) (take y ys) of
          Left _ -> combat memory' (xs <> [x, y]) ys
          Right _ -> combat memory' xs (ys <> [y, x])
      | x > y = combat memory' (xs <> [x, y]) ys
      | x < y = combat memory' xs (ys <> [y, x])
      where
        memory' = Set.insert (p1, p2) memory
