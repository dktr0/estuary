module Estuary.Languages.SvgOp (svgOp) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Types.Color
import Estuary.Types.Stroke
import Estuary.Types.Transform
import Estuary.Types.SvgOp

svgOp :: Text -> Either ParseError [SvgOp]
svgOp x = parse svgOpParser "SvgOp" $ T.unpack x

svgOpParser :: Parser [SvgOp]
svgOpParser = do
  whiteSpace
  ops <- semiSep ops
  eof
  return ops

ops :: Parser SvgOp
ops = choice [line, rect, circle, ellipse, triangle, polyline, polygon]

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

polyline :: Parser SvgOp
polyline = (reserved "polyline" >> return Polyline) <*> listOfDoubles <*> fill <*> stroke

polygon :: Parser SvgOp
polygon = (reserved "polygon" >> return Polygon) <*> listOfDoubles <*> fill <*> stroke

double :: Parser Double
double = choice [try float,fromIntegral <$> integer]

listOfDoubles :: Parser [Double]
listOfDoubles = do
  xs <- many1 double
  return xs

fill :: Parser Color
fill = do
  f <- option (RGBA 100 100 100 100) fill'
  return $ f

stroke :: Parser Stroke
stroke = do
  s <- option (Stroke (RGBA 100 100 100 100) 1 Butt Miter (DashArray 0 0)) (reserved "s" >> stroke')
  return $ s


stroke' :: Parser Stroke
stroke' = brackets $ do
  c <- option (RGBA 100 100 100 100) color
  t <- option 1 double
  lc <- option Butt lineCap
  lj <- option Miter lineJoin
  d <- option (DashArray 0 0) dashArray
  return $ Stroke c t lc lj d

color :: Parser Color
color =  parens $ do
    r <- double
    comma
    g <- double
    comma
    b <- double
    option "," comma
    a <- option 100 double
    return $ RGBA r g b a

fill' :: Parser Color
fill' = do
  char 'f'
  f <- color
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

dashArray :: Parser DashArray
dashArray = parens $ do
  x <- double
  comma
  y <- double
  return $ DashArray x y

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["line", "rect", "circle", "ellipse", "triangle", "polyline", "polygon", "s", "t"],
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
