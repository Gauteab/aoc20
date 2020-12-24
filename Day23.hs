{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Data.Digits (unDigits)
import Data.IntMap ((!))
import qualified Data.IntMap as IntMap
import Data.List (maximum)
import Relude
import qualified Prelude

main :: IO ()
main = do
  let input = [3, 6, 2, 9, 8, 1, 7, 5, 4]
  print $ unDigits 10 . take 8 $ playGame 100 input
  let input = [3, 6, 2, 9, 8, 1, 7, 5, 4] <> [10 .. 1000000]
  print $ product . take 2 $ playGame 10000000 input

playGame :: Int -> [Int] -> [Int]
playGame rounds input = takeCardsFrom 1 . fst $ iterateTo rounds (step (maximum input)) (initialMap, Prelude.head input)
  where
    initialMap = IntMap.fromList $ (Prelude.last input, Prelude.head input) : (zip <*> Prelude.tail $ input)

takeCardsFrom :: Int -> IntMap Int -> [Int]
takeCardsFrom i cards = (cards ! i) : takeCardsFrom (cards ! i) cards

step :: Int -> (IntMap Int, Int) -> (IntMap Int, Int)
step maximal (cards, current) =
  ( cards
      & ix destination .~ first
      & ix last .~ (cards ! destination)
      & ix current .~ (cards ! last)
  , cards ! last
  )
  where
    pickup@[first, second, last] = [cards ! current, cards ! first, cards ! second]
    destination = nextDestination (current - 1)
    nextDestination x
      | x == 0 = nextDestination maximal
      | x `notElem` pickup = x
      | otherwise = nextDestination (x - 1)

iterateTo :: Int -> (a -> a) -> a -> a
iterateTo n f a = go n 0 f a
  where
    go n i _ a | n == i = a
    go n i f a = go n (i + 1) f (f a)
