
module Estuary.Languages.Saludos (saludos) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

--saludos
-- <nombreDelSample><espacio><transformaciones>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  espacios
  char '¡'
  espacios
  s1 <- saludos'
  espacios
  s2 <- saludos'
  espacios
  s3 <- saludos'
  espacios
  s4 <- saludos'
  espacios
  char '!'
  espacios
  t1 <- transformaciones
  espacios
  t2 <- transformaciones
  espacios
  t3 <- transformaciones
  espacios
  t4 <- transformaciones
  espacios
  return $ t1 $ t2 $ t3 $ t4 $ nuestroTextoATidal $ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ " "

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s


espacios :: GenParser Char a String
espacios = many (oneOf " ")



saludos' :: GenParser Char a String
saludos' = choice [
        try (string "hola" >> espacios >> return "moog"),
        try (string "como estas" >> espacios >> return "arpy"),
        try (string "saludos" >> espacios >> return "bd"),
        try (descartarTexto >> return " ")
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

saludos :: String -> Either ParseError Tidal.ParamPattern
saludos s = parse exprStack "saludos" s
