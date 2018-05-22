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
  spaces'
  s <- pasion
  spaces'
  t <- sustantivos
  spaces'
  return $ t $ stringToTidalPattern s

stringToTidalPattern :: String -> Tidal.ParamPattern
stringToTidalPattern s = Tidal.s $ Tidal.p s

spaces' :: GenParser Char a String
spaces' = many (oneOf " ")

pasion :: GenParser Char a String
pasion = choice [
        try (string "voz" >> spaces' >> return "birds3"),
        try (string "pasion" >> spaces' >> return "blip"),
        try (string "paz" >> spaces' >> return "sax")
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

colombiaEsPasion :: String -> Tidal.ParamPattern
colombiaEsPasion s = either (const Tidal.silence) id $ parse exprStack "pasion" s
