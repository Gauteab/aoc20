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
      differences = map (uncurry (flip (-))) $ (zip <*> Prelude.tail) input'
  -- Part 1: (1856)
  print . product . map length . group . sort $ differences
  -- Part 2: (2314037239808)
  print . product . map (possibilities . length) . filter (any (/= 3)) $ group differences

possibilities :: Int -> Int
possibilities 1 = 1
possibilities 2 = 2
possibilities 3 = 4
possibilities 4 = 7
possibilities _ = error "I don't know man"
