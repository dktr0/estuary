module Estuary.Types.Name where

import Data.Text as T
import Data.Char
import Text.Parsec
import Text.Parsec.Text

type Name = Text
type Password = Text

-- names can start with any character, number, or symbol
-- and consist of any character, number, or symbol
-- and must not contain any spaces, tabs, newlines

nameIsLegal :: Text -> Bool
nameIsLegal x = not (T.any isControl x || T.any isSpace x )

nameOrPassword :: Parser Text
nameOrPassword = do
  x <- pack <$> many1 (satisfy (\x -> not (isControl x) && not (isSpace x)))
  spaces
  return x
