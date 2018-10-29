module Estuary.Languages.ColombiaEsPasion (colombiaEsPasion) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

-- <nombreDelSample><transformacion1><transformacion2>
--voz soacha
--voz motesta 4

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  espacios
  char '"'
  espacios
  s1 <- pasion
  espacios
  s2 <- pasion
  espacios
  s3 <- pasion
  espacios
  s4 <- pasion
  espacios
  char '"'
  espacios
  t1 <- sustantivos
  espacios
  t2 <- sustantivos
  espacios
  t3 <- sustantivos
  espacios
  t4 <- sustantivos
  espacios
  return $ t1 $ t2 $ t3 $ t4 $ nuestroTextoATidal $ s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ s4 ++ " "

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

espacios :: GenParser Char a String
espacios = many (oneOf " ")

pasion :: GenParser Char a String
pasion = choice [
        try (string "voz" >> espacios >> return "birds3"),
        try (string "pasion" >> espacios >> return "blip"),
        try (string "paz" >> espacios >> return "sax"),
        try (descartarTexto >> return " ")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

sustantivos :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
sustantivos = choice [
                try (string "educación" >> spaces >> fractional3 False >>= return . Tidal.slow),
                try (string "motesta" >> spaces >> fractional3 False >>= return . Tidal.fast),
                try (string "soacha" >> return Tidal.brak),
                try (descartarTexto >> return id)
                ]


exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

colombiaEsPasion :: String -> Either ParseError Tidal.ParamPattern
colombiaEsPasion s = parse exprStack "pasion" s
