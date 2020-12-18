{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Lens
import Relude hiding (many, (<|>))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (nat)
import qualified Prelude

main :: IO ()
main = do
  input <- traverse (parse expression "") . Prelude.lines <$> readFile "./input/day18"
  input' <- traverse (parse expression' "") . Prelude.lines <$> readFile "./input/day18"
  print (sum <$> input)
  print (sum <$> input')

expression :: Parser Int
expression = buildExpressionParser table (term expression)

expression' :: Parser Int
expression' = buildExpressionParser table' (term expression')

table = [[binary " * " (*) AssocLeft, binary " + " (+) AssocLeft]]
table' = [[binary " + " (+) AssocLeft], [binary " * " (*) AssocLeft]]

term e = (char '(' *> e <* char ')') <|> nat

binary name fun = Infix (fun <$ try (string name))
