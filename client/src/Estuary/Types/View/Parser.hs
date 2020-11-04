{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.View.Parser (viewParser,dumpView) where

import Text.Parsec
import Text.Parsec.Text
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Control.Monad.Identity (Identity)

import Estuary.Types.View

dumpView :: View -> Text
dumpView (Views xs) = "[" <> (T.intercalate "," $ fmap dumpView xs) <> "]"
dumpView (GridView cols rows vs) = showInt cols <> "x" <> showInt  rows <>  " [" <> vs' <> "]"
  where vs' = T.intercalate ","  $ fmap dumpView vs
dumpView (ViewDiv css v) = "{ " <> css <> " " <> dumpView v <> " }"
dumpView (StructureView x) = "structure:" <> showInt x
dumpView (LabelView x) = "label:" <> showInt x
dumpView (TextView x y) = "text:" <> showInt x <> " " <> showInt y
dumpView (SequenceView z) = "sequence:" <> showInt z
dumpView (BorderDiv v) = "border { " <> dumpView v <> "} "
dumpView TempoView = "tempo"
dumpView EnsembleStatusView = "ensembleStatus"
dumpView (RouletteView x rows) = "roulette:" <> showInt x <> " " <> showInt rows
dumpView AudioMapView = "audiomapview"

showInt :: Int -> Text
showInt x = showtParen (x < 0) (showt x)

viewsParser :: Parser View
viewsParser = do
  vs <- brackets $ commaSep viewParser
  return $ Views vs

gridViewParser :: Parser View
gridViewParser = do
  columns <- int
  lexeme (oneOf "xX")
  whiteSpace
  rows <- int
  vs <- brackets $ commaSep viewParser
  return $ GridView columns rows vs

viewParser :: Parser View
viewParser = do
  v <- choice [
    try gridViewParser,
    try viewsParser,
    try viewDiv,
    try borderDiv,
    try labelView,
    try structureView,
    try sequenceView,
    try ensembleStatusView,
    try tempo,
    try rouletteView,
    try audiomapview,
    textView
    ]
  return v

viewDiv = braces $ (ViewDiv <$> (T.pack <$> identifier) <*> viewsParser)
borderDiv = reserved "border" >> (braces $ (BorderDiv <$> viewsParser))
labelView = reserved "label" >> reservedOp ":" >> (LabelView <$> int)
structureView = reserved "structure" >> reservedOp ":" >> (StructureView <$> int)
sequenceView = reserved "sequence" >> reservedOp ":" >> (SequenceView <$> int)
ensembleStatusView = reserved "ensembleStatus" >> return EnsembleStatusView
tempo = reserved "tempo" >> return TempoView
textView = reserved "text" >> reservedOp ":" >> (TextView <$> int <*> int)
rouletteView = reserved "roulette" >> reservedOp ":" >> (RouletteView <$> int <*> int)
audiomapview = reserved "audiomap" >> return AudioMapView

int :: Parser Int
int = choice [
  parens $ (fromIntegral <$> integer),
  fromIntegral <$> integer
  ]

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
    "label","structure","sequenceView","textView","svgDisplayView",
    "canvasDisplayView", "x", "ensembleStatus", "border", "tempo", "roulette"
    ],
  P.reservedOpNames = [":"],
  P.caseSensitive = True
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
