module Estuary.Types.ViewsParser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import Data.List (intercalate)

import Estuary.Types.View

dumpView :: View -> String
dumpView (Views xs) = intercalate " " $ fmap dumpView xs
dumpView (ViewDiv css v) = "{ " ++ css ++ " " ++ dumpView v ++ " }"
dumpView (StructureView x) = "structure:" ++ showInt x
dumpView (LabelView x) = "label:" ++ showInt x
dumpView (TextView x y) = "textView:" ++ showInt x ++ " " ++ showInt y
dumpView (SvgDisplayView z) = "svgDisplayView:" ++ showInt z
dumpView (CanvasDisplayView z) = "canvasDisplayView:" ++ showInt z
dumpView (SequenceView z) = "sequenceView:" ++ showInt z

showInt :: Int -> String
showInt x | x >= 0 = show x
showInt x | otherwise = "(" ++ show x ++ ")"

topLevelViewsParser :: Parser View
topLevelViewsParser = do
  whiteSpace
  v <- viewsParser
  eof
  return v

viewsParser :: Parser View
viewsParser = Views <$> many1 viewParser

viewParser :: Parser View
viewParser = do
  v <- choice [
    try viewDiv,
    try labelView,
    try structureView,
    try sequenceView,
    try tidalTextView,
    try svgDisplayView,
    canvasDisplayView
    ]
  return v

viewDiv = braces $ (ViewDiv <$> identifier <*> viewsParser)
labelView = reserved "label" >> reservedOp ":" >> (LabelView <$> int)
structureView = reserved "structure" >> reservedOp ":" >> (StructureView <$> int)
sequenceView = reserved "sequenceView" >> reservedOp ":" >> (SequenceView <$> int)
tidalTextView = reserved "textView" >> reservedOp ":" >> (TextView <$> int <*> int)
svgDisplayView = reserved "svgDisplayView" >> reservedOp ":" >> (SvgDisplayView <$> int)
canvasDisplayView = reserved "canvasDisplayView" >> reservedOp ":" >> (CanvasDisplayView <$> int)

int :: Parser Int
int = choice [
  parens $ (fromIntegral <$> integer),
  fromIntegral <$> integer
  ]

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = [
    "label","structure","sequenceView","textView","svgDisplayView",
    "canvasDisplayView"
    ],
  P.reservedOpNames = [":"]
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
