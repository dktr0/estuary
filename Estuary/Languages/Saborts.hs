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
  espacios
  char ':'
  many (oneOf "!, @, #, $, %, ^, /, (, ), =, +, *, {, }, |, ;, ~, ?, ¿")
  espacios
  s1 <- sonidos
  s2 <- sonidos
  s3 <- sonidos
  s4 <- sonidos
  s5 <- sonidos
  s6 <- sonidos
  s7 <- sonidos
  s8 <- sonidos
  s9 <- sonidos
  s10 <- sonidos
  s11 <- sonidos
  s12 <- sonidos
  s13 <- sonidos
  s14 <- sonidos
  s15 <- sonidos
  s16 <- sonidos
  s17 <- sonidos
  s18 <- sonidos
  s19 <- sonidos
  s20 <- sonidos
  s21 <- sonidos
  s22<-  sonidos
  s23 <- sonidos
  s24 <- sonidos
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
  return $ t1 $t2 $t3 $t4 $ nuestroTextoATidal $ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ " " ++ s5 ++ " " ++ s6 ++ " " ++ s7 ++ " " ++ s8 ++ " " ++ s9 ++ " " ++ s10 ++ " " ++ s11 ++ " " ++ s12 ++ " " ++ s13 ++ " " ++ s14 ++ " " ++ s15 ++ " " ++ s16 ++ " " ++ s17 ++ " " ++ s18 ++ " " ++ s19 ++ " " ++ s20 ++ " " ++ s21 ++ " " ++ s22 ++ " " ++ s23 ++ " " ++ s24 ++ " "

  --zs <- many (oneOf "~")
  --t1 <- trans

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s


sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "[" >> espacios >> return "[" ),
        try (string "]" >> espacios >> return "]"),
        try (string "_" >> espacios >> return "~"),
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

-- sonidosConCorchetes :: GenParser Char a String
-- sonidosConCorchetes = do
--   espacios
--   char '['
--   espacios
--   s1 <- sonidosSinCorchetes
--   espacios
--   s2 <- sonidosSinCorchetes
--   espacios
--   s3 <- sonidosSinCorchetes
--   espacios
--   s4 <- sonidosSinCorchetes
--   espacios
--   char ']'
--   espacios
--   return $  "[" ++ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ "]"
--   -- c1 ++ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ c2
--
--
-- sonidos :: GenParser Char a String
-- sonidos = choice [
--            try sonidosSinCorchetes,
--            try sonidosConCorchetes
--            -- try (descartarTexto >> return " ")
--            ]

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
