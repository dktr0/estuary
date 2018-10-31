module Estuary.Languages.Alobestia (alobestia) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
-- ?
--por Gaby Dávila y Lenin Moncayo
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
        try (string "boom" >> espacios >> return "bd" ),
        try (string "clap" >> espacios >> return "cp" ),
        try (string "zap" >> espacios >> return "bass" ),
        try (descartarTexto >> return " ")
        ]


trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
              try (string "lento" >> spaces >> fractional3 False >>= return . Tidal.slow),
              try (string "distorsion" >> spaces >> fractional3 False >>= return . Tidal.trunc),
              try (string "densidad" >> spaces >> fractional3 False >>= return . Tidal.density),
              try (string "rapido" >> spaces >> fractional3 False >>= return . Tidal.fast),
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

alobestia :: String -> Either ParseError Tidal.ParamPattern
alobestia s = parse exprStack "Alobestia" s
