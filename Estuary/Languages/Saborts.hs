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
  many (oneOf "!, #, $, %, ^, /, (, ), =, +, *, {, }, |, ;, ~, ?, ¿")
  espacios
  n <- stringAnumeros
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
  s25 <- sonidos
  s26 <- sonidos
  s27 <- sonidos
  s28 <- sonidos
  s29 <- sonidos
  s30 <- sonidos
  s31 <- sonidos
  s32 <- sonidos
  s33 <- sonidos
  s34 <- sonidos
  s35 <- sonidos
  s36 <- sonidos
  s37 <- sonidos
  s38 <- sonidos
  s39 <- sonidos
  s40 <- sonidos
  s41 <- sonidos
  s42 <- sonidos
  s43 <- sonidos
  s44 <- sonidos
  s45 <- sonidos
  s46<-  sonidos
  s47 <- sonidos
  s48 <- sonidos
  espacios
  char '!'
  espacios
  t1 <- trans
  espacios
  -- t2 <- trans
  -- espacios
  -- t3 <- trans
  -- espacios
  -- t4 <- trans
  -- espacios
  return $ t1 $ nuestroTextoATidal (s1  ++ s2 ++ s3  ++ s4  ++ s5  ++ s6  ++ s7  ++ s8 ++  s9  ++ s10  ++ s11 ++ s12 ++ s13 ++ s14 ++ s15 ++ s16 ++ s17 ++ s18 ++ s19 ++ s20 ++ s21 ++ s22  ++ s23 ++ s24 ++ s25  ++ s26 ++ s27  ++ s28  ++ s29  ++ s30  ++ s31  ++ s32 ++  s33  ++ s34  ++ s35 ++ s36 ++ s37 ++ s38 ++ s39 ++ s40 ++ s42 ++ s42 ++ s43 ++ s44 ++ s45 ++ s46  ++ s47 ++ s48) Tidal.# Tidal.up n

stringAnumeros :: GenParser Char a (Tidal.Pattern Double)
stringAnumeros = choice [
        try parseString,
        try (descartarTexto >> return 0)
        ]


parseString :: Tidal.Parseable b => GenParser Char a (Tidal.Pattern b)
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    spaces
    return $ Tidal.p x

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s


sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "[" >> espacios >> return "[" ),
        try (string "]" >> espacios >> return "]"),
        try (string "_" >> espacios >> return "~"),
        try (string ">>" >> espacios >> return "*"),
        try (string "<<" >> espacios >> return "/"),
        try (string "@" >> return ":"),
        try (string "0" >> return "0"),
        try (string "1" >> return "1"),
        try (string "2" >> return "2"),
        try (string "3" >> return "3"),
        try (string "4" >> return "4"),
        try (string "5" >> return "5"),
        try (string "6" >> return "6"),
        try (string "7" >> return "7"),
        try (string "8" >> return "8"),
        try (string "9" >> return "9"),
        try (string " " >> return " "),
        try (string "ñ" >> return "geom"),
        try (string "a" >> return "arpy"),
        try (string "b" >> return "bombo"),
        try (string "c" >> return "clap"),
        try (string "d" >> return "congas"),
        try (string "e" >> return "efectos"),
        try (string "f" >> return "drumtraks"),
        try (string "g" >> return "guiros"),
        try (string "h" >> return "contras"),
        try (string "i" >> return "hh"),
        try (string "j" >> return "casio"),
        try (string "k" >> return "bongo"),
        try (string "l" >> return "timbal"),
        try (string "m" >> return "bajo" ),
        try (string "n" >> return "bleep" ),
        try (string "o" >> return "cowbell"),
        try (string "p" >> return "ade"),
        try (string "t" >> return "tarolas"),
        try (string "v" >> return "voces" ),
        try (string "x" >> return "efectos"),
        try (string "y" >> return "tom"),
        try (descartarTexto >> return " ")
        ]

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "q" >> spaces >> fractional3 False  >>= return . Tidal.fast),
         try (string "r" >> return Tidal.rev),
         try (string "s">> spaces >> fractional3 False   >>= return . Tidal.slow),
         try (string "w" >> spaces >>  return Tidal.brak),
         try (string "z" >> spaces  >> int >>= return . Tidal.gap),
         try (string "u" >> return Tidal.palindrome),
         try (string ">" >> spaces >> fractional3 False >>= return . Tidal.density),
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

saborts :: String -> Tidal.ParamPattern
saborts s = either (const Tidal.silence) id $ parse exprStack "unNombreparaTuLenguage" s
