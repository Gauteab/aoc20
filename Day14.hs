{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Data.Digits
import qualified Data.Map as Map
import Relude
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (nat)

data Instruction = NewMask Mask | Assignment Int Int deriving (Show)
type Mask = [Maybe Int]
data State' = State' {_mask :: Mask, _memory :: Map Int Int} deriving (Show)

makeLenses ''State'

main :: IO ()
main = do
  Right input <- parse parser "" <$> readFile "./input/day14"
  print $ part1 input --
  print $ part2 input --

part1, part2 :: [Instruction] -> Int
part1 = sum . _memory . flip execState (State' [] Map.empty) . traverse evaluate
part2 = sum . _memory . flip execState (State' [] Map.empty) . traverse evaluate'

parser :: Parser [Instruction]
parser = choice [newMask, assignment] `sepEndBy` space
  where
    newMask = NewMask <$> (try (string "mask = ") *> count 36 mask)
    mask = choice [char 'X' $> Nothing, char '1' $> Just 1, char '0' $> Just 0]
    assignment = Assignment <$> (try (string "mem[") *> nat <* string "] = ") <*> nat

evaluate (NewMask newMask) = modify $ mask .~ newMask
evaluate (Assignment a x) = do
  currentMask <- gets _mask
  let x' = applyMask currentMask x
  modify $ memory . at a ?~ x'

evaluate' (NewMask newMask) = modify $ mask .~ newMask
evaluate' (Assignment a x) = do
  currentMask <- gets _mask
  let addresses = applyMask' currentMask a
  for_ addresses $ \a' -> do
    modify $ memory . at a' ?~ x

applyMask' :: Mask -> Int -> [Int]
applyMask' mask i = do
  a <- traverse (maybe [0, 1] pure) $ zipWith f mask (pad 36 $ digits 2 i)
  pure $ unDigits 2 a
  where
    f (Just 0) y = Just y
    f m _ = m

asDigits :: Iso' Int [Int]
asDigits = iso (digits 2) (unDigits 2)

pad :: Num a => Int -> [a] -> [a]
pad n xs = replicate (n - length xs) 0 <> xs

applyMask :: Mask -> Int -> Int
applyMask mask = asDigits %~ zipWith f mask . pad 36
  where
    f Nothing y = y
    f (Just x) _ = x
