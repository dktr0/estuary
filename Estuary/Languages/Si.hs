module Estuary.Languages.Si (si) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

-- <nombreDelSample><transformacion1><transformacion2>
--"Nose" Pegudo -5 Tortuga 28
--voz motesta 4

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  spaces'
  s <- bancoDeSonidos
  spaces'
  t1 <- ordenesDentrada1
  spaces'
  t2 <- ordenesDentrada2
  spaces
  return $ t1 $ t2 $ stringToTidalPattern s

stringToTidalPattern :: String -> Tidal.ParamPattern
stringToTidalPattern s = Tidal.s $ Tidal.p s

spaces' :: GenParser Char a String
spaces' = many (oneOf " ")

bancoDeSonidos :: GenParser Char a String
bancoDeSonidos = choice [
        try (string "Nose" >> spaces' >> return "bd"),
        try (string "Willy" >> spaces' >> return "hh"),
        try (string "Gracioso" >> spaces' >> return "sn"),
        try (string "Elefante" >> spaces' >> return "bass")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

ordenesDentrada1 :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
ordenesDentrada1 = choice [
                try (string "Pegado"  >> spaces >> int >>= return . Tidal.iter),
                try (string "lejos" >> spaces >> fractional3 False >>= return . Tidal.density),
                try (descartarTexto >> return id)
                ]

ordenesDentrada2 :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
ordenesDentrada2 = choice [
                try (string "Tortuga" >> spaces >> fractional3 False >>= return . Tidal.slow),
                try (string "Comadreja" >> spaces >> fractional3 False >>= return . Tidal.fast),
                  try (descartarTexto >> return id)
                ]


exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

si :: String -> Tidal.ParamPattern
si s = either (const Tidal.silence) id $ parse exprStack "unNombre" s
