module Estuary.Languages.Test2 (test2) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Data.Bool (bool)
import qualified Sound.Tidal.Context as Tidal

--how do we want the syntax of our language?
--what words we want on our Languages to label sounds and transformations?
--sustantivos de muebles == sounds
--silencio == tilde
--adjectives == transformations
-- mesa ~ silla ~
--e.g. mesa ~ azul
--Parser String too?

muebles ::  GenParser Char a String
muebles = choice [
      try (string "mesa" >> return "sugar"),
      try (string "silla" >> return "able"),
      try (string "cama" >> return "sn")
      ]

spaces' :: GenParser Char a String
spaces' = many (oneOf " ")

parseExpr ::  GenParser Char a Tidal.ParamPattern
parseExpr = do
  xs <- spaces'
  ys <- muebles
  return $ stringToTidalPattern ys

stringToTidalPattern :: String -> Tidal.ParamPattern --"~ jazz/1"
stringToTidalPattern sampleName = Tidal.s $ Tidal.p sampleName

stackOfExpr ::  GenParser Char a Tidal.ParamPattern
stackOfExpr = do
  xs <- many parseExpr
  return $ Tidal.stack xs

test2 :: String -> Tidal.ParamPattern
test2 s = either (const Tidal.silence) id $ parse stackOfExpr "test2" s
