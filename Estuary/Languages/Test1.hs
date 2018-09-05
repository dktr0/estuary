module Estuary.Languages.Test1 (test1) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
import Data.List (intercalate)

--saborts
-- <emoticon> <nombre sonido> <transf1> <parametros>
-- :) q! w10
-- cyril
lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
--coloca aquí los parsers
  espacios
  char ':'
  many (oneOf "!, @, #, $, %, ^, /, (, ), =, +, *, [,], {, }, |, ;, ~, ?, ¿")
  espacios
  s1 <- sonidos
  s2 <- sonidos
  s3 <- sonidos
  s4 <- sonidos
  s5 <- sonidos
  s6 <- sonidos
  s7 <- sonidos
  s8 <- sonidos
  espacios
  char '!'
  espacios
  t1 <- trans
  espacios
  t2 <- trans
  espacios
  t3 <- trans
  espacios
  t4 <- trans
  espacios
  return $ t1 $t2 $t3 $t4 $ nuestroTextoATidal $ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ " "

  --zs <- many (oneOf "~")[]
  --t1 <- trans

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s


sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "piña" >> espacios >> return "foo" ),
        try (string "reñir" >> espacios >> return "feel" ),
        try (string "hazaña" >> espacios >> return "jazz" ),
        try (string "montaña" >> espacios >> return "printshot" ),
        try (string "niño" >> espacios >> return "industrial" ),
        try (string "piraña" >> espacios >> return "koy" ),
        try (descartarTexto >> return " ")
        ]


trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "soñemos" >> spaces >>  return Tidal.brak),
         try (string "teñiste" >> spaces >> fractional3 False  >>= return . Tidal.fast),
         try (string "otoño">> spaces >> fractional3 False   >>= return . Tidal.slow),
         try (descartarTexto >> return id)
                ]

numeros ::  Char -> Char
numeros '|' = '1'
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

test1 :: String -> Tidal.ParamPattern
test1 s = either (const Tidal.silence) id $ parse exprStack "unNombreparaTuLenguage" s
