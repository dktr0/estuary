module Estuary.Languages.Sucixxx (sucixxx) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.ParamPatternable (parseBP')

-- suci xxx
--por Chakala, Dani, Carolina y Juana
-- <nombre sonido> <transf1> <parametros>

lengExpr :: GenParser Char a Tidal.ControlPattern
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

nuestroTextoATidal ::  String  -> Tidal.ControlPattern
nuestroTextoATidal s = Tidal.s $ parseBP' s

inicio :: GenParser Char a String
inicio = choice [
        try (string "putita-" ),
        try (string "perrita-"),
        try (string "tu sicaria-"),
        try (string "mala mujer-"),
        try (string "amorfada-"),
        try (string "gata fiera-"),
        try (string "torta golosa-"),
        try (string "feminasty-")
        ]

sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "comeme" >> espacios >> return "fire" ),
        try (string "dame" >> espacios >> return "bass2" ),
        try (string "azotame" >> espacios >> return "808:2" ),
        try (string "rompeme" >> espacios >> return "gabba" ),
        try (string "barre el piso" >> espacios >> return "notes" ),
        try (string "interpelame" >> espacios >> return "casio" ),
        try (string "encadename" >> espacios >> return "metal" ),
        try (string "aborta" >> espacios >> return "arpy" ),
        try (descartarTexto >> return " ")
        ]


trans :: GenParser Char a (Tidal.ControlPattern -> Tidal.ControlPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "suave" >> spaces >> fractional3 False >>= return . Tidal.slow),
         try (string "suave suavecito" >> spaces >> fractional3 False >>= return . Tidal.slow),
         try (string "duro" >> spaces >> fractional3 False >>= return . Tidal.fast),
         try (string "más más" >> spaces >> fractional3 False >>= return . Tidal.fast),
         try (string "con flow" >> spaces >> return Tidal.rev),
         try (string "con el pelo" >> spaces >> int >>= return . Tidal.chop),
         try (string "con lengua" >> spaces >> int >>= return . Tidal.striate),
         try (descartarTexto >> return id)
                ]

--descartar espacios
espacios :: GenParser Char a String
espacios = many (oneOf " ")

--descartar texto
descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

exprStack :: GenParser Char a Tidal.ControlPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

sucixxx :: String -> Either ParseError Tidal.ControlPattern
sucixxx s = parse exprStack "suci xxx" s
