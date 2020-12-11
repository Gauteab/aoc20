{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Relude

main :: IO ()
main = do
  input <- fmap toString . lines <$> readFileText "./input/day3"
  let width = length $ input ^?! ix 0
      grid = makeGrid input
  print $ trees width grid (3, 1) -- 193
  print . product $ trees width grid <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] -- 1355323200

trees :: Int -> Map (Int, Int) Char -> (Int, Int) -> Int
trees width grid (right, down) = length . filter (== '#') . flip unfoldr (0, 0) $ \position ->
  grid ^? ix position <&> (,bimap ((`mod` width) . (+ right)) (+ down) position)

makeGrid :: [String] -> Map (Int, Int) Char
makeGrid input = fromList $ do
  (y, squares) <- zip [0 ..] input
  (x, square) <- zip [0 ..] squares
  pure ((x, y), square)
