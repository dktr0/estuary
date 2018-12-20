module Estuary.Languages.CanvasOp (canvasOp) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import Estuary.Types.CanvasOp
import Estuary.Types.Color

canvasOp :: String -> Either ParseError [CanvasOp]
canvasOp = parse canvasOpParser "CanvasOp"

canvasOpParser :: Parser [CanvasOp]
canvasOpParser = do
  whiteSpace
  ops <- semiSep ops
  eof
  return ops

ops :: Parser CanvasOp
ops = choice [ lineTo, moveTo, rect, tri, strokeStyle, fillStyle]

lineTo :: Parser CanvasOp
lineTo = (reserved "lineTo" >> return LineTo) <*> double <*> double

moveTo :: Parser CanvasOp
moveTo = (reserved "moveTo" >> return MoveTo) <*> double <*> double

rect :: Parser CanvasOp
rect = (reserved "rect" >> return Rect) <*> double <*> double <*> double <*> double

tri :: Parser CanvasOp
tri = (reserved "tri" >> return Tri) <*> double <*> double <*> double <*> double <*> double <*> double

strokeStyle :: Parser CanvasOp
strokeStyle = (reserved "strokeStyle" >> return StrokeStyle) <*> color

fillStyle :: Parser CanvasOp
fillStyle = (reserved "fillStyle" >> return FillStyle) <*> color

color :: Parser Color
color = parens $ do
  r <- double
  comma
  g <- double
  comma
  b <- double
  comma
  a <- double
  return $ RGBA r g b a

double :: Parser Double
double = choice [try float,fromIntegral <$> integer]

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["lineTo","moveTo","rect","tri","strokeStyle","fillStyle"],
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
