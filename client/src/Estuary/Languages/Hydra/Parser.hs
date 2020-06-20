{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Hydra.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad.Identity (Identity)

import Estuary.Languages.Hydra.Types

----

hydra :: Text -> Either ParseError Statement
hydra s = parse hydraStatement "hydra" s

hydraStatement :: Parser Statement
hydraStatement = choice [
  sourceOut
  ]

sourceOut :: Parser Statement
sourceOut = do
  s <- parserSource
  o <- parserOut
  return $ sourceOutputToStatement s o

-- renderOut :: Parser Statement
-- renderOut
-- render()

sourceOutputToStatement :: Source -> Output -> Statement
sourceOutputToStatement s o = Out s o

parserSource :: Parser Source
parserSource = constant

constant :: Parser Source
constant = do
  n <- double
  return $ Constant n

parserOut :: Parser Output
parserOut = do
  reserved "out"
  o <- parens $ parserOutput
  return $ o

parserOutput :: Parser Output
parserOutput = choice [
  reserved "" >> return O0,
  reserved "O0" >> return O0,
  reserved "O1" >> return O1,
  reserved "O2" >> return O2,
  reserved "O3" >> return O3
  ]


---------
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
  P.commentStart = "{-",
  P.commentEnd = "-}",
  P.commentLine = "--",
  P.nestedComments = False,
  P.identStart = letter <|> char '_',
  P.identLetter = alphaNum <|> char '_',
  P.opStart = oneOf "+*:@<>~=%",
  P.opLetter = oneOf "+*:@<>~=%",
  P.reservedNames = [
    "out"
    ],
  P.reservedOpNames = [],
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
