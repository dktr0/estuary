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
  o <- choice [
    try $ parens output,
    parens whiteSpace >> return O0
    ]
  return $ Out s o

output :: Parser Output
output = choice [
  reserved "o0" >> return O0,
  reserved "o1" >> return O1,
  reserved "o2" >> return O2,
  reserved "o3" >> return O3
  ]

renderStatement :: Parser Statement
renderStatement = Render <$> (reserved "render" >> parens output)

source :: Parser Source
source = choice [
  Osc <$> functionWithSources "osc",
  Solid <$> functionWithSources "solid",
  Gradient <$> functionWithSources "gradient",
  Noise <$> functionWithSources "noise",
  Shape <$> functionWithSources "shape",
  Voronoi <$> functionWithSources "voronoi",
  Brightness <$> source <*> methodWithParameterLists "brightness",
  Contrast <$> source <*> methodWithParameterLists "contrast",
  Colorama <$> source <*> methodWithParameterLists "colorama"
  ]

functionWithSources :: String -> Parser [Parameters]
functionWithSources x = reserved x >> try (parens (commaSep argument))

methodWithParameterLists :: String -> Parser [Parameters]
methodWithParameterLists x = do
  reservedOp "."
  reserved x
  try (parens (commaSep argument))

argument :: Parser Parameters
argument = choice [
  list,
  constantDouble
  ]

list :: Parser Parameters
list = Parameters <$> brackets (commaSep double)

constantDouble :: Parser Parameters
constantDouble = (Parameters . return) <$> double

int :: Parser Int
int = fromIntegral <$> integer

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
