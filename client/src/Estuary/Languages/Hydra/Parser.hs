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
  x <- choice [
    Osc <$> functionWithParameters "osc",
    Solid <$> functionWithParameters "solid",
    Gradient <$> functionWithParameters "gradient",
    Noise <$> functionWithParameters "noise",
    Shape <$> functionWithParameters "shape",
    Voronoi <$> functionWithParameters "voronoi"
    ]
  fs <- many $ choice [
    Brightness <$> methodWithParameters "brightness",
    Contrast <$> methodWithParameters "contrast",
    Colorama <$> methodWithParameters "colorama",
    Color <$> methodWithParameters "color",
    Invert <$> methodWithParameters "invert",
    Luma <$> methodWithParameters "luma",
    Posterize <$> methodWithParameters "posterize",
    Saturate <$> methodWithParameters "saturate",
    Shift <$> methodWithParameters "shift",
    Thresh <$> methodWithParameters "tresh",
    Kaleid <$> methodWithParameters "kaleid",
    Pixelate <$> methodWithParameters "pixelate",
    Repeat <$> methodWithParameters "repeat",
    RepeatX <$> methodWithParameters "repeatX",
    RepeatY <$> methodWithParameters "repeatY",
    Rotate <$> methodWithParameters "rotate",
    Scale <$> methodWithParameters "scale",
    Scroll <$> methodWithParameters "scroll",
    ScrollX <$> methodWithParameters "scrollX",
    ScrollY <$> methodWithParameters "scrollY"

    ]
  return $ (foldl (.) id fs) x

functionWithParameters :: String -> Parser [Parameters]
functionWithParameters x = try $ reserved x >> parens (commaSep parameters)

methodWithParameters :: String -> Parser [Parameters]
methodWithParameters x = try $ reservedOp "." >> reserved x >> parens (commaSep parameters)

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
