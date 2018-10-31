module Estuary.Languages.LaCalle (laCalle) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
--lima
-- <nombre sonido> <transf1> <parametros>

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
        try (string "hola choche" >> espacios >> return "sitar" ),
        try (string "unas chelas" >> espacios >> return "ifdrums" ),
        try (string "mi germa" >> espacios >> return "metal" ),
        try (string "vamos a" >> espacios >> return "casio"),
        try (descartarTexto >> return " ")
        ]

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "tu manyas" >> spaces >> fractional3 False  >>= return . Tidal.slow),
         try (string "bien helenas" >> spaces >> fractional3 False  >>= return . Tidal.fast),
         try (string "palta con el">> spaces >> int >>= return . Tidal.iter),
         try (string "mi cerro" >> spaces >> int >>= return . Tidal.chop),
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

laCalle :: String -> Either ParseError Tidal.ParamPattern
laCalle s = parse exprStack "LaCalle" s
