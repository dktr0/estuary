module Estuary.Languages.Test1 (test1) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
--trans
-- <nombre de sonido> trans_<transformacion>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  espacios
  s <- sonidos
  espacios
  string "trans_"
  espacios
  t <- trans
  espacios
  return $ t $ nuestroTextoATidal $ s
  --nuestroTextoATidal $ "~" ++ " "  ++ s1 ++ " " ++ s2 ++ " " ++ "~"

nuestroTextoATidal ::  String  -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

espacios :: GenParser Char a String
espacios = many (oneOf " ")

sonidos :: GenParser Char a String
sonidos = choice [
        try (string "bombo" >> espacios >> return "bd"),
        try (string "maraca" >> espacios >> return "crow")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
                try (string "fast" >> spaces >> fractional3 False >>= return . Tidal.fast),
                try (string "density" >> spaces >> fractional3 False >>= return . Tidal.density),
                try (descartarTexto >> return id)
                ]

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

test1 :: String -> Tidal.ParamPattern
test1 s = either (const Tidal.silence) id $ parse exprStack "unNombre" s
