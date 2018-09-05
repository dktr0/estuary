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
  t3 <- stringASpeed
  espacios
  n3 <- numeros
  espacios
  t4 <- stringADelay
  espacios
  n4 <- numeros
  espacios
  return $ t1 $ t2 $ nuestroTextoATidal (s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ " ") Tidal.# Tidal.speed t3 Tidal.# Tidal.delay t4

nuestroTextoATidal ::  String  -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "hola coche" >> espacios >> return "diphone" ),
        try (string "unas chelas" >> espacios >> return "dist" ),
        try (string "mi germa" >> espacios >> return "hh" ),
        try (string "vamos a" >> espacios >> return "mash2"),
        try (descartarTexto >> return " ")
        ]

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "tu manyas" >> spaces >> fractional3 False  >>= return . Tidal.slow),
         -- try (string "bien helenas" >> spaces >> fractional3 False  >>= return . Tidal.fast),
         -- try (string "palta con el tombo">> spaces >> int >>= return . Tidal.iter),
         try (string "mi cerro" >> spaces >> int >>= return . Tidal.chop),
         try (descartarTexto >> return id)
                ]

stringADelay :: GenParser Char a (Tidal.Pattern Double)
stringADelay = choice [
        try parseDelay,
        try (descartarTexto >> return 0)
        ]

stringASpeed :: GenParser Char a (Tidal.Pattern Double)
stringASpeed = choice [
        try parseSpeed,
        try (descartarTexto >> return 1)
        ]

parseDelay :: Tidal.Parseable b => GenParser Char a (Tidal.Pattern b)
parseDelay = do
    string "bien helenas"
    spaces
    x <- many(noneOf "bien helenas")
    spaces
    return $ Tidal.p x


parseSpeed :: Tidal.Parseable b => GenParser Char a (Tidal.Pattern b)
parseSpeed = do
    string "palta con el"
    spaces
    x <- many(noneOf "palta con el")
    spaces
    return $ Tidal.p x

numeros :: GenParser Char a Double
numeros = choice [
   try (string "mi jato" >> return 7),
   try (string "y vamos a jatear" >> return 5),
   try (string "tombo" >> return 3),
   try (descartarTexto >> return 0)
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

laCalle :: String -> Tidal.ParamPattern
laCalle s = either (const Tidal.silence) id $ parse exprStack "unNombreparaTuLenguage" s
