module Estuary.Languages.Natural (natural) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

--natural
-- <nombre de sonido> <transformacion> <paramatrosd de Transf>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  espacios
  char 'a'
  espacios
  s1 <- animal
  espacios
  s2 <- animal
  espacios
  s3 <- animal
  espacios
  s4 <- animal
  espacios
  t1 <- accion
  espacios
  t2 <- accion
  espacios
  t3 <- accion
  espacios
  t4 <- accion
  espacios
  return $ t1 $ t2 $ t3 $ t4 $ nuestroTextoATidal $ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ " "

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

espacios :: GenParser Char a String
espacios = many (oneOf " ")

animal :: GenParser Char a String
animal = choice [
        try (string "El Cóndor" >> espacios >> return "sax"),
        try (string "El Hombre" >> espacios >> return "pluck"),
        try (string "El León" >> espacios >> return "bass"),
        try (descartarTexto >> return " ")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

accion :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
accion = choice [
                try (string "vuelo"  >> espacios >> return Tidal.rev),
                try (string "caza" >> espacios >> int >>= return . Tidal.striate),
                try (string "rugido" >> espacios >> fractional3 False >>= return . Tidal.slow),
                try (descartarTexto >> return id)
                ]

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

natural :: String -> Either ParseError Tidal.ParamPattern
natural s = parse exprStack "natural" s
