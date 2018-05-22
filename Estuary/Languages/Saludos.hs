
module Estuary.Languages.Saludos (saludos) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

-- <nombreDelSample><espacio><transformaciones>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  spaces'
  s <- saludos'
  spaces'
  t <- transformaciones
  spaces'
  return $ t $ stringToTidalPattern s

stringToTidalPattern :: String -> Tidal.ParamPattern
stringToTidalPattern s = Tidal.s $ Tidal.p s


spaces' :: GenParser Char a String
spaces' = many (oneOf " ")



saludos' :: GenParser Char a String
saludos' = choice [
        try (string "hola" >> spaces' >> return "moog"),
        try (string "como estas" >> spaces' >> return "arpy"),
        try (string "saludos" >> spaces' >> return "bd")
        ]

transformaciones :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
transformaciones = do
     t <- charATransformaciones
     return t

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

charATransformaciones :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
charATransformaciones = choice [
                try (string "que" >> return Tidal.brak),
                try (string "todo bien" >> spaces >> int >>= return . Tidal.chop),
                try (descartarTexto >> return id)
                ]

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

saludos :: String -> Tidal.ParamPattern
saludos s = either (const Tidal.silence) id $ parse exprStack "saludos" s
