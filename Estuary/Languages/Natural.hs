module Estuary.Languages.Natural (natural) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal

--natural
-- <nombre de sonido> <transformacion> <paramatrosd de Transf>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  sonido <- animal
  transf <- accion
  --param <- adjetivos
  return $ transf $ nuestroTextoATidal sonido

nuestroTextoATidal :: String -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

espacios :: GenParser Char a String
espacios = many (oneOf " ")

animal :: GenParser Char a String
animal = choice [
        try (string "El Cóndor" >> espacios >> return "sax"),
        try (string "El Hombre" >> espacios >> return "pluck"),
        try (string "El León" >> espacios >> return "bass")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

accion :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
accion = choice [
                try (string "vuelo"  >> espacios >> return Tidal.rev),
                try (string "caza" >> espacios >> int >>= return . Tidal.striate),
                try (string "rugido" >> espacios >> fractional3 False >>= return . Tidal.slow),
                try (descartarTexto >> return id)
                ]

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

natural :: String -> Tidal.ParamPattern
natural s = either (const Tidal.silence) id $ parse exprStack "unNombre" s
