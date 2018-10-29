module Estuary.Languages.Puntoyya (puntoyya) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
--punto y ya
-- <nombre sonido> <transf1> <parametros>
--  .-. o 2

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
--coloca aquí los parsers
  espacios
  char '.'
  espacios
  s1 <- sonidos
  espacios
  s2 <- sonidos
  espacios
  s3 <- sonidos
  espacios
  s4 <- sonidos
  espacios
  char '.'
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
        try (char '_' >> espacios >> return "cp" ),
        try (char '-' >> espacios >> return "arpy" ),
        try (char '|' >> espacios >> return "bass" ),
        try (char '"' >> espacios >> return "hh" ),
        try (descartarTexto >> return " ")
        ]

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "o" >> spaces >> fractional3 False >>= return . Tidal.fast),
         try (string "oo" >> spaces >> fractional3 False >>= return . Tidal.slow),
         try (string "ooo" >> spaces >> int >>= return . Tidal.iter),
         try (string "oooo" >> spaces >> int >>= return . Tidal.chop),
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

puntoyya :: String -> Either ParseError Tidal.ParamPattern
puntoyya s = parse exprStack "puntoyya" s
