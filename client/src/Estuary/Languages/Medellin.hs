module Estuary.Languages.Medellin (medellin) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
--trans
-- <nombre de sonido> trans_<transformacion>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  espacios
  char 's'
  espacios
  s1 <- sonidos
  espacios
  s2 <- sonidos
  espacios
  s3 <- sonidos
  espacios
  s4 <- sonidos
  espacios
  t1 <- trans
  espacios
  t2 <- trans
  espacios
  t3 <- trans
  espacios
  t4 <- trans
  espacios
  return $ t1 $ t2 $ t3 $ t4 $ nuestroTextoATidal $ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ " "

nuestroTextoATidal ::  String  -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

espacios :: GenParser Char a String
espacios = many (oneOf " ")

sonidos :: GenParser Char a String
sonidos = choice [
        try (string "bombo" >> espacios >> return "bd"),
        try (string "maraca" >> espacios >> return "crow"),
        try (descartarTexto >> return " ")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

trans' :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans' = choice [
                try (string "fast" >> spaces >> fractional3 False >>= return . Tidal.fast),
                try (string "density" >> spaces >> fractional3 False >>= return . Tidal.density),
                try (descartarTexto >> return id)
                ]

trans'' :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans'' =  do
  espacios
  string "trans_"
  t <- trans'
  espacios
  return t

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans =  choice [
               try trans'',
               try (descartarTexto >> return id)
                ]

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

medellin :: String -> Either ParseError Tidal.ParamPattern
medellin s = parse exprStack "Medellin" s
