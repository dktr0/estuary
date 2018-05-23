module Estuary.Languages.Machete (machete) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal
--trans
-- <nombre de sonido> trans_<transformacion>

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
--coloca aquí los parsers
  espacios
  s <- sonidos
  espacios
  char "#"
  t <- transf
  espacios
  return $ t $ nuestroTextoATidal s

nuestroTextoATidal ::  String  -> Tidal.ParamPattern
nuestroTextoATidal s = Tidal.s $ Tidal.p s

sonidos :: GenParser Char a String
sonidos = choice [
        --coloca aqui los nombres de tus muestras de audio
        --ej. try (string "bombo" >> espacios >> "bd")
        try (string "~" >> espacios >> "~"),
        try (string "machete" >> espacios >> "bass")
        ]

trans :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
trans = choice [
               --coloca aqui los nombres de tus transformaciones
              --ej. try (string "fast" >> spaces >> fractional3 False >>= return . Tidal.fast),
                try (descartarTexto >> return id),
                try (string "rapidin" >> espacios >> fractional3 False >>= return . Tidal.fast  )
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

machete :: String -> Tidal.ParamPattern
machete s = either (const Tidal.silence) id $ parse exprStack "machete" s
