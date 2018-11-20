module Estuary.Languages.SuperContinent (superContinent) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import Estuary.Types.SvgOp

superContinent :: String -> Either ParseError [SvgOp]
superContinent = parse superContinentParser "SuperContinent"

superContinentParser :: Parser [SvgOp]
superContinentParser = whiteSpace >> choice [line, rect] >>= return . (:[])

line :: Parser SvgOp
line = (reserved "line" >> return Line) <*> double <*> double <*> double <*> double

rect :: Parser SvgOp
rect = (reserved "rect" >> return Rect) <*> double <*> double <*> double <*> double

double :: Parser Double
double = choice [try float,fromIntegral <$> integer]

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["line","rect"],
  P.reservedOpNames = []
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
