{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Relude

main :: IO ()
main = do
  input <- map toString . lines <$> readFileText "./input/17"
  let grid = makeGrid input
      grid' = makeGrid' input
  print $ Set.size <$> (iterate part1 grid ^? ix 6) -- 207
  print $ Set.size <$> (iterate part2 grid' ^? ix 6) -- 2308

part2 :: Set (Int, Int, Int, Int) -> Set (Int, Int, Int, Int)
part2 grid = Set.fromList $ do
  let xMax = fromJust $ Set.lookupMax (Set.map (^. _1) grid)
  let xMin = fromJust $ Set.lookupMin (Set.map (^. _1) grid)
  let yMax = fromJust $ Set.lookupMax (Set.map (^. _2) grid)
  let yMin = fromJust $ Set.lookupMin (Set.map (^. _2) grid)
  let zMax = fromJust $ Set.lookupMax (Set.map (^. _3) grid)
  let zMin = fromJust $ Set.lookupMin (Set.map (^. _3) grid)
  let wMax = fromJust $ Set.lookupMax (Set.map (^. _4) grid)
  let wMin = fromJust $ Set.lookupMin (Set.map (^. _4) grid)
  x <- [xMin -1 .. xMax + 1]
  y <- [yMin -1 .. yMax + 1]
  z <- [zMin -1 .. zMax + 1]
  w <- [wMin -1 .. wMax + 1]
  let position = (x, y, z, w)
  let an = activeNeighbors' grid position
  if Set.member position grid
    then [position | an `elem` [2, 3]]
    else [position | an == 3]

part1 :: Set (Int, Int, Int) -> Set (Int, Int, Int)
part1 grid = Set.fromList $ do
  let xMax = fromJust $ Set.lookupMax (Set.map (^. _1) grid)
  let xMin = fromJust $ Set.lookupMin (Set.map (^. _1) grid)
  let yMax = fromJust $ Set.lookupMax (Set.map (^. _2) grid)
  let yMin = fromJust $ Set.lookupMin (Set.map (^. _2) grid)
  let zMax = fromJust $ Set.lookupMax (Set.map (^. _3) grid)
  let zMin = fromJust $ Set.lookupMin (Set.map (^. _3) grid)
  x <- [xMin -1 .. xMax + 1]
  y <- [yMin -1 .. yMax + 1]
  z <- [zMin -1 .. zMax + 1]
  let position = (x, y, z)
  let an = activeNeighbors grid position
  if Set.member position grid
    then [position | an `elem` [2, 3]]
    else [position | an == 3]

activeNeighbors grid position = length $ filter (`Set.member` grid) (neighbors position)
activeNeighbors' grid position = length $ filter (`Set.member` grid) (neighbors' position)

neighbors' (x, y, z, w) = do
  p@[x', y', z', w'] <- replicateM 4 [0, 1, -1]
  guard $ not (all (== 0) p)
  pure (x + x', y + y', z + z', w + w')

neighbors (x, y, z) = do
  p@[x', y', z'] <- replicateM 3 [0, 1, -1]
  guard $ not (all (== 0) p)
  pure (x + x', y + y', z + z')

makeGrid' :: [String] -> Set (Int, Int, Int, Int)
makeGrid' = Set.map (\(x, y, z) -> (x, y, z, 0)) . makeGrid

makeGrid :: [String] -> Set (Int, Int, Int)
makeGrid input = fromList $ do
  (y, squares) <- zip [0 ..] input
  (x, square) <- zip [0 ..] squares
  guard (square == '#')
  pure (x, y, 0)
