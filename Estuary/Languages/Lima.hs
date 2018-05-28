module Estuary.Languages.Lima (lima) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
--lima
-- <nombre sonido> <transf1> <parametros>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
--coloca aquí los parsers
  espacios
  s <- sonidos
  espacios
  t1 <- trans
  espacios
  return $ t1 $ nuestroTextoATidal s

nuestroTextoATidal ::  String  -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "hola coche" >> espacios >> return "sitar" ),
        try (string "unas chelas" >> espacios >> return "ifdrums" ),
        try (string "mi germa" >> espacios >> return "metal" ),
        try (string "vamos a" >> espacios >> return "casio")
        ]

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "tu manyas mi jato" >> spaces >> fractional3 False  >>= return . Tidal.slow),
         try (string "bien helenas y vamos a jatear" >> spaces >> fractional3 False  >>= return . Tidal.fast),
         try (string "palta con el tombo">> spaces >> int >>= return . Tidal.iter),
         try (string "mi cerro causa" >> spaces >> int >>= return . Tidal.chop),
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

lima :: String -> Tidal.ParamPattern
lima s = either (const Tidal.silence) id $ parse exprStack "unNombreparaTuLenguage" s
