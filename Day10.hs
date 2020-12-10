{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Data.List (maximum)
import Relude
import qualified Prelude

main :: IO ()
main = do
  Just input <- traverse (readMaybe @Int . toString) . lines <$> readFileText "./input/day10"
  let input' = 0 : sort input <> [maximum input + 3]
      differences = map (uncurry subtract) $ (zip <*> Prelude.tail) input'
  -- Part 1: (1856)
  print . product . map length . group . sort $ differences
  -- Part 2: (2314037239808)
  print . product . map (possibilities . length) . filter (any (/= 3)) $ group differences

possibilities :: Int -> Int
possibilities n = n * (n - 1) `div` 2 + 1
