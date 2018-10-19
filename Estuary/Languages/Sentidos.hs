module Estuary.Languages.Sentidos (sentidos) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

--sentidos
-- <nombreDelSample><transformacion1><transformacion2>
--melodioso agitado 5 tristeza 6

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  --coloca aquí los parsers
  espacios
  char '"'
  espacios
  s1 <- sentidos'
  espacios
  s2 <- sentidos'
  espacios
  s3 <- sentidos'
  espacios
  s4 <- sentidos'
  espacios
  char '"'
  espacios
  t1 <- transformacion
  espacios
  t2 <- transformacion
  espacios
  t3 <- transformacion
  espacios
  t4 <- transformacion
  espacios
  return $ t1 $ t2 $ t3 $ t4 $ nuestroTextoATidal $ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ " "

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

espacios :: GenParser Char a String
espacios = many (oneOf " ")

sentidos' :: GenParser Char a String
sentidos' = choice [
        try (string "rocoso" >> espacios >> return "flick"),
        try (string "melodioso" >> espacios >> return "sid"),
        try (string "ondulado" >> espacios >> return "tabla"),
        try (descartarTexto >> return " ")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

adjetivos :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
adjetivos = choice [
                try (string "agitado" >> espacios >> fractional3 False >>= return . Tidal.fast),
                try (string "calma" >>  espacios >> fractional3 False >>= return . Tidal.density),
                try (descartarTexto >> return id)
                ]

sentimientos :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
sentimientos = choice [
                try (string "tristeza"  >> espacios >> int >>= return . Tidal.chop),
                try (string "felicidad" >> espacios >> fractional3 False >>= return . Tidal.trunc),
                try (string "amor" >> espacios >> int >>= return . Tidal.iter),
                try (descartarTexto >> return id)
                ]

sentimientos' :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
sentimientos' = do
   char '#'
   espacios
   s <- sentimientos
   espacios
   return s

adjetivos' :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
adjetivos' = do
   char '#'
   espacios
   s <- adjetivos
   espacios
   return s

transformacion :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
transformacion = choice [
                 try adjetivos',
                 try sentimientos',
                 try (descartarTexto >> return id)
                  ]
{--transformaciones' :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
transformaciones = do
     t <- charATransformaciones
     return t
--}

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

sentidos :: String -> Either ParseError Tidal.ParamPattern
sentidos s = parse exprStack "sentimientos" s
