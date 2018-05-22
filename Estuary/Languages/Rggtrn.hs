module Estuary.Languages.Test1 (test1) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
test1
--natural
-- <nombre de sonido> <transformacion> <paramatrosd de Transf>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  s <- sonidos
  char "~"
  t <- t
  --param <- adjetivos
  return $ transf $ nuestroTextoATidal sonido

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

espacios :: GenParser Char a String
espacios = many (oneOf " ")

sonidos :: GenParser Char a String
sonidos = choice [
        try (string "El Cóndor" >> espacios >> return "sax"),
        try (string "El Hombre" >> espacios >> return "pluck"),
        try (string "El León" >> espacios >> return "bass")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

t :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
t = choice [
                try (string "vuelo"  >> espacios >> return Tidal.rev),
                try (string "caza" >> espacios >> int >>= return . Tidal.striate),
                try (string "rugido" >> espacios >> fractional3 False >>= return . Tidal.slow),
                try (descartarTexto >> return id)
                ]

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

test1 :: String -> Tidal.ParamPattern
test1 s = either (const Tidal.silence) id $ parse exprStack "unNombre" s
