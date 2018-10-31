module Estuary.Languages.Crudo (crudo) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

--Crudo
-- "<samples><samples><samples><samples>" <espacio> <transformación><parametros>


lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
--coloca aquí los parsers
  espacios
  char '"'
  espacios
  s1 <- sonidos
  espacios
  s2 <- sonidos
  espacios
  s3 <- sonidos
  espacios
  s4 <- sonidos
  espacios
  char '"'
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

sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "trueno" >> espacios >> return "bd"),
        try (string "río" >> espacios >> return "sn"),
        try (string "cascada" >> espacios >> return "wind"),
        try (string "volcán" >> espacios >> return "stomp"),
        try (string "rama" >> espacios >> return "hh"),
        try (string "viento" >> espacios >> return "wind"),
        try (string "cueva" >> espacios >> return "short"),
        try (descartarTexto >> return " ")
        ]

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
        try (string "eco" >> return Tidal.palindrome),
        try (string "oscuridad" >> spaces >> fractional3 False >>= return . Tidal.slow),
        try (string "salvaje" >> spaces >> fractional3 False >>= return . Tidal.density),
        try (string "este" >> spaces >> fractional3 False >>= return . Tidal.fast),
        try (string "oeste" >> spaces >> fractional3 False >>= return . Tidal.trunc),
        try (descartarTexto >> return id)
        ]

--descartar espacios
espacios :: GenParser Char a String
espacios = many (oneOf " ")

--descartar texto
descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

crudo :: String -> Either ParseError Tidal.ParamPattern
crudo s = parse exprStack "Crudo" s
