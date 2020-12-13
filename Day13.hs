{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.List.Extra (minimumOn)
import Relude hiding ((<|>))
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (nat)

main :: IO ()
main = do
  Right (target, buses) <- parse parser "" <$> readFile "./input/day13"
  print $ uncurry (*) $ minimumOn snd $ map ((,) <*> subtract target . firstTime target) (catMaybes buses) -- 4782
  print $ showEquation $ makeEquation buses -- Paste Result Into Wolfram Alpha -> 1118684865113056

makeEquation :: [Maybe Int] -> [(Int, Int)]
makeEquation = mapMaybe f . zip [0 ..]
  where
    f (_, Nothing) = Nothing
    f (n, Just x) = Just (n, x)

showEquation :: [(Int, Int)] -> String
showEquation = intercalate "," . map f
  where
    f (i, v) = "Mod[x+" <> show i <> "," <> show v <> "]=0"

firstTime :: Int -> Int -> Int
firstTime target bus = ceiling (fromIntegral target / fromIntegral bus) * bus

parser :: Parser (Int, [Maybe Int])
parser = (,) <$> nat <* space <*> sepBy (Just <$> nat <|> Nothing <$ char 'x') (char ',')
