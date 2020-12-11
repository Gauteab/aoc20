{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Relude
import Text.Parsec
import Text.Parsec.Text

data Input = Input Int Int Char String

part1 :: [Input] -> Int
part1 = length . filter p
  where
    p (Input x y c s) =
      let l = length $ filter (== c) s
       in l >= x && l <= y

part2 :: [Input] -> Int
part2 = length . filter p
  where
    p (Input x y c s) =
      let (a, b) = (s ^?! ix (x - 1), s ^?! ix (y - 1))
       in a /= b && (a == c || b == c)

parser :: Parser Input
parser = Input <$> int <* char '-' <*> int <* space <*> anyChar <* string ": " <*> many1 letter <* newline
  where
    int = fromMaybe undefined . readMaybe <$> many1 digit

main :: IO ()
main = do
  Right input <- runParser (many1 parser) () "" <$> readFileText "./input/day2"
  print $ part1 input -- 591
  print $ part2 input -- 335
