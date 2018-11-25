module Estuary.Languages.SvgOp (svgOp) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import Estuary.Types.Color
import Estuary.Types.Stroke
import Estuary.Types.SvgOp

svgOp :: String -> Either ParseError [SvgOp]
svgOp = parse svgOpParser "SvgOp"

svgOpParser :: Parser [SvgOp]
svgOpParser = do
  whiteSpace
  ops <- semiSep ops
  eof
  return ops

ops :: Parser SvgOp
ops = choice [line, rect, circle, ellipse, triangle]

line :: Parser SvgOp
line = (reserved "line" >> return Line) <*> double <*> double <*> double <*> double <*> stroke

rect :: Parser SvgOp
rect = (reserved "rect" >> return Rect) <*> double <*> double <*> double <*> double <*> fill <*> stroke

circle :: Parser SvgOp
circle = (reserved "circle" >> return Circle) <*> double <*> double <*> double <*> fill <*> stroke

ellipse :: Parser SvgOp
ellipse = (reserved "ellipse" >> return Ellipse) <*> double <*> double <*> double <*> double <*> fill <*> stroke

triangle :: Parser SvgOp
triangle = (reserved "triangle" >> return Triangle) <*> double <*> double <*> double <*> double <*> double <*> double <*> fill <*> stroke

double :: Parser Double
double = choice [try float,fromIntegral <$> integer]

-- point :: Parser (Double, Double)
-- point = do
--   p1 <- double
--   p2 <- double
--   return $ show p1 ++ "," ++ show p2

stroke :: Parser Stroke
stroke = do
  c <- option (RGBA 100 100 100 100) color
  t <- option 1 double
  lc <- option Butt lineCap
  lj <- option Miter lineJoin
  return $ Stroke c t lc lj

color :: Parser Color
color = parens $ do
  r <- double
  comma
  g <- double
  comma
  b <- double
  option "," comma
  a <- option 100 double
  return $ RGBA r g b a


fill :: Parser Color
fill = do
  f <- option (RGBA 100 100 100 100) color
  return $ f

lineCap :: Parser LineCap
lineCap = choice [
  reserved "Butt" >> return Butt,
  reserved "Square" >> return Square,
  reserved "RoundCap" >> return RoundCap
  ]

lineJoin :: Parser LineJoin
lineJoin = choice [
  reserved "Miter" >> return Miter,
  reserved "RoundJoin" >> return RoundJoin,
  reserved "Bevel" >> return Bevel
  ]


tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["line", "rect", "circle", "ellipse", "triangle"],
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
