module Estuary.Languages.Imagina (imagina) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
-- suci xxx
-- <nombre sonido> <transf1> <parametros>


lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
--coloca aquí los parsers
  espacios
  inicio
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

inicio :: GenParser Char a String
inicio = choice [
        try (string "imagina" ),
        try (string "sueña"),
        try (string "medita")
        ]

sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "el agua" >> espacios >> return "pluck:3" ),
        try (string "las hojas" >> espacios >> return "wind" ),
        try (string "el pájaro" >> espacios >> return "birds3" ),
        try (descartarTexto >> return " ")
        ]


trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
              try (string "del río" >> return Tidal.palindrome),
              try (string "cayendo" >> spaces >> fractional3 False >>= return . Tidal.slow),
              try (string "del mar" >> spaces >> fractional3 False >>= return . Tidal.density),
              try (string "del árbol" >> return Tidal.palindrome),
              try (string "caer" >> spaces >> fractional3 False >>= return . Tidal.slow),
              try (string "crecer" >> spaces >> fractional3 False >>= return . Tidal.density),
              try (string "cantando" >> spaces >> fractional3 False >>= return . Tidal.fast),
              try (string "volando" >> spaces >> fractional3 False >>= return . Tidal.density),
              try (string "comiendo" >> spaces >> fractional3 False >>= return . Tidal.trunc),
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

imagina :: String -> Either ParseError Tidal.ParamPattern
imagina s = parse exprStack "imagina" s
