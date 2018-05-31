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
        try (string "g" >> espacios >> return "guiro" ),
        try (string "b" >> espacios >> return "bombo"),
        try (string "v" >> espacios >> return "kick_v" ),
        try (string "d" >> espacios >> return "kick_boom"),
        try (string "k" >> espacios >> return "kick"),
        try (string "m" >> espacios >> return "bass_m" ),
        try (string "h" >> espacios >> return "hi1"),
        try (string "i" >> espacios >> return "hi2"),
        try (string "t" >> espacios >> return "snare_1"),
        try (string "a" >> espacios >> return "timbales"),
        try (string "c" >> espacios >> return "clap1"),
        try (string "o" >> espacios >> return "conga"),
        try (string "e" >> espacios >> return "geom"),
        try (descartarTexto >> return " ")
        ]


trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "w" >> spaces >>  return Tidal.brak),
         try (string "q" >> spaces >> fractional3 False  >>= return . Tidal.fast),
         try (string "s">> spaces >> fractional3 False   >>= return . Tidal.slow),
         try (string "z" >> spaces  >> int >>= return . Tidal.gap ),
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
