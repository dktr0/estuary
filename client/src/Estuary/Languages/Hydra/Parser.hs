{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Hydra.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad.Identity (Identity)

import Estuary.Languages.Hydra.Types
import Estuary.Languages.Hydra.Test

----

parseHydra :: Text -> Either ParseError [Statement]
parseHydra s = parse hydra "hydra" s

hydra :: Parser [Statement]
hydra = do
  whiteSpace
  xs <- semiSep statement
  eof
  return xs

statement :: Parser Statement
statement = choice [ outStatement, renderStatement ]

outStatement :: Parser Statement
outStatement = do
  s <- source
  reservedOp "."
  reserved "out"
  o <- output
  return $ Out s o

output :: Parser Output
output = try $ parens $ choice [
  reserved "o0" >> return O0,
  reserved "o1" >> return O1,
  reserved "o2" >> return O2,
  reserved "o3" >> return O3,
  whiteSpace >> return O0
  ]

renderStatement :: Parser Statement
renderStatement = Render <$> (reserved "render" >> output)

source :: Parser Source
source = do
  x <- choice [ -- a source is a single "atomic" Source...
    functionWithParameters "osc" Osc,
    functionWithParameters "solid" Solid,
    functionWithParameters "gradient" Gradient,
    functionWithParameters "noise" Noise,
    functionWithParameters "shape" Shape,
    functionWithParameters "voronoi" Voronoi
    ]
  fs <- many $ choice [ -- ...to which zero or more transformations [Source -> Source] are applied.
    methodWithParameters "brightness" Brightness,
    methodWithParameters "contrast" Contrast,
    methodWithParameters "colorama" Colorama,
    methodWithParameters "color" Color,
    methodWithParameters "invert" Invert,
    methodWithParameters "luma" Luma,
    methodWithParameters "posterize" Posterize,
    methodWithParameters "saturate" Saturate,
    methodWithParameters "shift" Shift,
    methodWithParameters "tresh" Thresh,
    methodWithParameters "kaleid" Kaleid,
    methodWithParameters "pixelate" Pixelate,
    methodWithParameters "repeat" Repeat,
    methodWithParameters "repeatX" RepeatX,
    methodWithParameters "repeatY" RepeatY,
    methodWithParameters "rotate" Rotate,
    methodWithParameters "scale" Scale,
    methodWithParameters "scroll" Scroll,
    methodWithParameters "scrollX" ScrollX,
    methodWithParameters "scrollY" ScrollY,
    methodWithSourceAndParameters "modulate" Modulate
    ]
  return $ (foldl (.) id $ reverse fs) x -- compose the transformations into a single transformation and apply to source

functionWithParameters :: String -> ([Parameters] -> Source) -> Parser Source
functionWithParameters funcName constructor = try $ do
  reserved funcName
  ps <- parens $ commaSep parameters
  return $ constructor ps

methodWithParameters :: String -> ([Parameters] -> Source -> Source) -> Parser (Source -> Source)
methodWithParameters methodName constructor = try $ do
  reservedOp "."
  reserved methodName
  ps <- parens $ commaSep parameters
  return $ constructor ps

methodWithSourceAndParameters :: String -> (Source -> [Parameters] -> Source -> Source) -> Parser (Source -> Source)
methodWithSourceAndParameters methodName constructor = try $ do
  reservedOp "."
  reservedOp methodName
  (s,ps) <- parens $ do
    s <- source
    ps <- (comma >> commaSep1 parameters) <|> return []
    return (s,ps)
  return $ constructor s ps

parameters :: Parser Parameters
parameters = choice [
  Parameters <$> try (brackets (commaSep double)),
  (Parameters . return) <$> double
  ]

double :: Parser Double
double = choice [
  symbol "-" >> double >>= return . (* (-1)),
  try float,
  try $ fromIntegral <$> integer
  ]

---------

tokenParser :: P.GenTokenParser Text () Identity
tokenParser = P.makeTokenParser $ P.LanguageDef {
  P.commentStart = "/*",
  P.commentEnd = "*/",
  P.commentLine = "//",
  P.nestedComments = False,
  P.identStart = letter <|> char '_',
  P.identLetter = alphaNum <|> char '_',
  P.opStart = oneOf ".",
  P.opLetter = oneOf ".",
  P.reservedNames = [
    "out","render",
    "osc","solid","gradient","noise","shape","voronoi",
    "brightness", "contrast", "colorama",
    "o0","o1","o2","o3"
    ],
  P.reservedOpNames = ["."],
  P.caseSensitive = False
  }

identifier = P.identifier tokenParser
reserved = P.reserved tokenParser
operator = P.operator tokenParser
reservedOp = P.reservedOp tokenParser
charLiteral = P.charLiteral tokenParser
stringLiteral = P.stringLiteral tokenParser
natural = P.natural tokenParser
integer = P.integer tokenParser
float = P.float tokenParser
naturalOrFloat = P.naturalOrFloat tokenParser
decimal = P.decimal tokenParser
hexadecimal = P.hexadecimal tokenParser
octal = P.octal tokenParser
symbol = P.symbol tokenParser
lexeme = P.lexeme tokenParser
whiteSpace = P.whiteSpace tokenParser
parens = P.parens tokenParser
braces = P.braces tokenParser
angles = P.angles tokenParser
brackets = P.brackets tokenParser
semi = P.semi tokenParser
comma = P.comma tokenParser
colon = P.colon tokenParser
dot = P.dot tokenParser
semiSep = P.semiSep tokenParser
semiSep1 = P.semiSep1 tokenParser
commaSep = P.commaSep tokenParser
commaSep1 = P.commaSep1 tokenParser
