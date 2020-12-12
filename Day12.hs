{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude
import Text.Parsec (char, choice, parse, sepEndBy, space)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (nat)

main :: IO ()
main = do
  Right input <- parse parser "" <$> readFile "./input/day12"
  print $ input
  print $ part1 input --
  print $ part2 input --

forward (wx, wy) (px, py) n = (px + wx * n, py + wy * n)

rotate' Left' w n = rotate' Right' w (4 - n)
rotate' _ (x, y) (fromIntegral -> n) =
  ( (x * c) + (y * s)
  , ((- x) * s) + (y * c)
  )
  where
    s = round $ sin ((pi / 180) * n * 90)
    c = round $ cos ((pi / 180) * n * 90)

move cardinal p n = case cardinal of
  North -> p & _2 +~ n
  South -> p & _2 -~ n
  East -> p & _1 +~ n
  West -> p & _1 -~ n

rotate Right' n c = fromEnum c & ((`mod` 4) . (+ n)) & toEnum
rotate Left' n c = fromEnum c & ((`mod` 4) . subtract n) & toEnum

data Instruction = Move Cardinal Int | Rotate Direction Int | Forward Int deriving (Show)
data Cardinal = North | East | South | West deriving (Show, Enum)
data Direction = Right' | Left' deriving (Show)

parser :: Parser [Instruction]
parser = instruction `sepEndBy` space
  where
    instruction =
      choice
        [ Move <$> cardinal <*> nat
        , Rotate <$> direction <*> ((`div` 90) <$> nat)
        , Forward <$> (char 'F' *> nat)
        ]
    direction = Right' <$ char 'R' <|> Left' <$ char 'L'
    cardinal =
      choice
        [ North <$ char 'N'
        , West <$ char 'W'
        , East <$ char 'E'
        , South <$ char 'S'
        ]

manhattan = on (+) abs

part1 :: [Instruction] -> Int
part1 = uncurry manhattan . snd . foldl' step (East, (0, 0))
  where
    step (c', p) (Move c n) = (c', move c p n)
    step (c, p) (Forward n) = (c, move c p n)
    step (c, p) (Rotate r n) = (rotate r n c, p)

part2 :: [Instruction] -> Int
part2 = uncurry manhattan . snd . foldl' step ((10, 1), (0, 0))
  where
    step (waypoint, p) (Move c' n) = (move c' waypoint n, p)
    step (waypoint, p) (Forward n) = (waypoint, forward waypoint p n)
    step (waypoint, p) (Rotate r n) = (rotate' r waypoint n, p)
