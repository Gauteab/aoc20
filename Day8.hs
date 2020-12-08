{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Relude
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

type Program = Map Int (String, Int)

main :: IO ()
main = do
  Right input <- traverse (parse parser "" . toString) . lines <$> readFileText "./input/day8"
  let instructions = Map.fromList $ zip [0 ..] input
  print $ part1 instructions -- 1949
  print $ part2 instructions -- 2092

parser :: Parser (String, Int)
parser = (,) <$> many1 letter <* space <*> int

part1 :: Program -> Maybe Int
part1 instructions = snd <$> firstRepeatOn fst (iterate (`step` instructions) (0, 0))

step :: (Int, Int) -> Program -> (Int, Int)
step (i, a) instructions = case instructions Map.! i of
  ("nop", _) -> (i + 1, a)
  ("acc", v) -> (i + 1, a + v)
  ("jmp", v) -> (i + v, a)

part2 :: Program -> Int
part2 instructions = firstTerminating (length instructions) (possiblePrograms instructions)

possiblePrograms :: Program -> [Program]
possiblePrograms instructions = jumps <&> \i -> instructions & ix i . _1 .~ "nop"
  where
    jumps :: [Int]
    jumps = map fst . Map.toList . Map.filter ((== "jmp") . fst) $ instructions --

firstTerminating :: Int -> [Program] -> Int
firstTerminating target programs = go (((0, 0),) <$> programs)
  where
    go :: [((Int, Int), Program)] -> Int
    go programs =
      let newStates = uncurry step <$> programs
       in case find ((== target) . fst) newStates of
            Nothing -> go (zip newStates (snd <$> programs))
            Just (_, a) -> a

-- lib
firstRepeatOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
firstRepeatOn project = either Just (const Nothing) . foldM f Set.empty
  where
    f seen x =
      let var = project x
       in if Set.member var seen
            then Left x
            else Right $ Set.insert var seen
