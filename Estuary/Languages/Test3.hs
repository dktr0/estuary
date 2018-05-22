module Estuary.Languages.Test1 (test1) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

--how do we want the syntax of our language?
--what words do we want on our lanuages to label sounds and transformations?
--silencio = tilde
--sounds = emoticons
--transformations = characters like ?, ", *
--e.g. ":) ~" # "^"lui

emoticonLangExpr :: GenParser Char a Tidal.ParamPattern
emoticonLangExpr = do
  spaces'
  t <- eTransformations
  spaces'
  e <- emoticons
  return $ t $ stringToTidalPattern e

stringToTidalPattern :: String -> Tidal.ParamPattern --":) ~" # "^"
stringToTidalPattern s = Tidal.s $ Tidal.p s

emoticons :: GenParser Char a String
emoticons = choice [
        try (string "jessica" >> return "moog"),
        try (string "emilio" >> return "arpy"),
        try (string "marianne" >> return "bd")
        ]

spaces' :: GenParser Char a String
spaces' = many (oneOf " ")

eTransformations :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
eTransformations = do
     t <- charToTransformation
     return t

charToTransformation :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
charToTransformation = string "b" >> return Tidal.brak


exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many emoticonLangExpr
   return $ Tidal.stack expr

test1 :: String -> Tidal.ParamPattern
test1 s = either (const Tidal.silence) id $ parse exprStack "test1" s
