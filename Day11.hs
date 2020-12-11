{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import qualified Data.Map as Map
import Relude
import qualified Prelude

main :: IO ()
main = do
  input <- makeGrid . map toString . lines <$> readFileText "./input/day11"
  print $ part1 input -- 2346
  print $ part2 input -- 2111

step n adjacent grid = Map.mapWithKey f grid
  where
    f p 'L' | 0 == adjacent (== '#') grid p = '#'
    f p '#' | n <= adjacent (== '#') grid p = 'L'
    f _ v = v

adjacent p grid (x, y) = length . filter p . mapMaybe (`Map.lookup` grid) $ do
  x' <- [x, x + 1, x -1]
  y' <- [y, y + 1, y -1]
  guard $ not (x' == x && y == y')
  [(x', y')]

adjacent' p grid position = length . filter p $ do
  x' <- [0, 1, -1]
  y' <- [0, 1, -1]
  guard $ not (x' == 0 && 0 == y')
  let f p@(x, y) = Map.lookup p grid <&> (,(x + x', y + y'))
  maybeToList . find (/= '.') . drop 1 $ unfoldr f position

gridFixedPoint n adjacent =
  fmap (length . Map.filter (== '#') . fst)
    . find (uncurry (==))
    . (zip <*> Prelude.tail)
    . iterate (step n adjacent)

part1 :: Map (Int, Int) Char -> Maybe Int
part1 = gridFixedPoint 4 adjacent

part2 :: Map (Int, Int) Char -> Maybe Int
part2 = gridFixedPoint 5 adjacent'

makeGrid :: [String] -> Map (Int, Int) Char
makeGrid input = fromList $ do
  (y, squares) <- zip [0 ..] input
  (x, square) <- zip [0 ..] squares
  pure ((x, y), square)
