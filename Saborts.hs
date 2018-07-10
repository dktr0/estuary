module Estuary.Languages.Saborts (saborts) where

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

  --zs <- many (oneOf "~")
  --t1 <- trans

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s


sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "g" >> espacios >> return "drumtraks" ),
        try (string "b" >> espacios >> return "bd"),
        try (string "v" >> espacios >> return "bd:1" ),
        try (string "d" >> espacios >> return "bd:2"),
        try (string "k" >> espacios >> return "bd:3"),
        try (string "m" >> espacios >> return "bass:1" ),
        try (string "h" >> espacios >> return "hh27"),
        try (string "i" >> espacios >> return "hh:7"),
        try (string "t" >> espacios >> return "sn:1"),
        try (string "a" >> espacios >> return "sn:2"),
        try (string "c" >> espacios >> return "cp:1"),
        try (string "o" >> espacios >> return "drum"),
        try (string "e" >> espacios >> return "drum:1"),
        try (descartarTexto >> return " ")
        ]


trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "w" >> spaces >>  return Tidal.brak),
         try (string "q" >> spaces >> fractional3 False  >>= return . Tidal.fast),
         try (string "s">> spaces >> fractional3 False   >>= return . Tidal.slow),
         try (string "z" >> spaces  >> int >>= return . Tidal.gap),
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

saborts :: String -> Tidal.ParamPattern
saborts s = either (const Tidal.silence) id $ parse exprStack "unNombreparaTuLenguage" s
