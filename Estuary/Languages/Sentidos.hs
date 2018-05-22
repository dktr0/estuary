module Estuary.Languages.Sentidos (sentidos) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Number
import qualified Sound.Tidal.Context as Tidal


-- <nombreDelSample><transformacion1><transformacion2>
--melodioso agitado 5 tristeza 6

lengExpr :: GenParser Char a Tidal.ParamPattern
lengExpr = do
  spaces'
  s <- sentidos'
  spaces'
  p <- adjetivos
  spaces'
  t <- sentimientos
  spaces'
  return $ p $ t $  stringToTidalPattern s

stringToTidalPattern :: String -> Tidal.ParamPattern
stringToTidalPattern s = Tidal.s $ Tidal.p s

spaces' :: GenParser Char a String
spaces' = many (oneOf " ")

sentidos' :: GenParser Char a String
sentidos' = choice [
        try (string "rocoso" >> spaces' >> return "flick"),
        try (string "melodioso" >> spaces' >> return "sid"),
        try (string "ondulado" >> spaces' >> return "tabla")
        ]

descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

adjetivos :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
adjetivos = choice [
                try (string "agitado" >> spaces >> fractional3 False >>= return . Tidal.fast),
              --  try (string "calma" >>  fractional3 False >>= return . Tidal.density),
                --try (string "ansioso" >> spaces >> int >>= return . Tidal.striate)
                try (descartarTexto >> return id)
                ]

sentimientos :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
sentimientos = choice [
                try (string "tristeza"  >> spaces >> int >>= return . Tidal.chop),
                --try (string "tristeza" >> spaces >> fractional3 False >>= return . Tidal.trunc),
                --try (string "amor" >> spaces >> int >>= return . Tidal.iter)
                try (descartarTexto >> return id)
                ]

sentimientos' :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
sentimientos' = do
   char '#'
   spaces'
   s <- sentimientos
   spaces'
   return s

{--transformaciones' :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
transformaciones = do
     t <- charATransformaciones
     return t
--}

exprStack :: GenParser Char a Tidal.ParamPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

sentidos :: String -> Tidal.ParamPattern
sentidos s = either (const Tidal.silence) id $ parse exprStack "sentimientos" s
