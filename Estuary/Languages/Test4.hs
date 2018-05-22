module Estuary.Languages.Test4 (test4) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
--natural
-- <nombre de sonido> <transformacion> <paramatrosd de Transf>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  espacios
  xs <- many (oneOf silencio)
  espacios
  s1 <- sonidos
  espacios
  t <- t
  --param <- adjetivos
  return $ t $ nuestroTextoATidal  $ xs ++ " "  ++ s1
  --nuestroTextoATidal $ "~" ++ " "  ++ s1 ++ " " ++ s2 ++ " " ++ "~"

nuestroTextoATidal ::  String  -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

silencio :: GenParser Char a String
silencio = choice [
         try (string "~" >> espacios >> return "~"),
         try (descartarTexto >> return " ")
         ]

espacios :: GenParser Char a String
espacios = many (oneOf " ")

sonidos :: GenParser Char a String
sonidos = choice [
        try (string "sax" >> espacios >> return "sax"),
        try (string "pluck" >> espacios >> return "pluck"),
        try (string "bass" >> espacios >> return "bass")
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

test4 :: String -> Tidal.ParamPattern
test4 s = either (const Tidal.silence) id $ parse exprStack "unNombre" s
