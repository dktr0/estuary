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
dumpView EmptyView = "empty"
dumpView (Div css vs) = "div \"" <> css <> "\"" <> dumpViews vs
dumpView (Views vs) = dumpViews vs
dumpView (Paragraph vs) = "paragraph " <> dumpViews vs
dumpView (BorderDiv vs) = "border " <> dumpViews vs
dumpView (Link url vs) = "link \"" <> url <> "\" " <> dumpViews vs
dumpView (BulletPoints vs) = "bulletpoints " <> dumpViews vs
dumpView (GridView cols rows vs) = showInt cols <> "x" <> showInt rows <> " " <> dumpViews vs
-- dumpView (Text t) = ...
dumpView (LabelView x) = "label " <> showInt x
dumpView (StructureView x) = "structure " <> showInt x
dumpView (CodeView x y) = "code " <> showInt x <> " " <> showInt y
dumpView (SequenceView z) = "sequence " <> showInt z
-- dumpView (Example tn txt) = ...
dumpView EnsembleStatusView = "ensembleStatus"
dumpView TempoView = "tempo"
dumpView (RouletteView x rows) = "roulette " <> showInt x <> " " <> showInt rows
dumpView AudioMapView = "audiomap"
dumpView (StopWatchView z) = "stopwatch " <> showInt z
dumpView _ = " "

dumpViews :: [View] -> Text
dumpViews vs = "[" <> (T.intercalate "," $ fmap dumpView vs) <> "]"

showInt :: Int -> Text
showInt x = showtParen (x < 0) (showt x)

viewParser :: Parser View
viewParser = choice [
  try $ EmptyView <$ reserved "empty",
  try $ reserved "div" >> (Div <$> (T.pack <$> stringLiteral) <*> viewsParser),
  try $ Views <$> viewsParser,
  try $ reserved "paragraph" >> (Paragraph <$> viewsParser),
  try $ reserved "border" >> (BorderDiv <$> viewsParser),
  try $ reserved "link" >> (Link <$> (T.pack <$> stringLiteral) <*> viewsParser),
  try $ reserved "bulletpoints" >> (BulletPoints <$> viewsParser),
  try gridViewParser,
  -- currently not parsing Text
  try $ reserved "label" >> (LabelView <$> int),
  try $ reserved "structure" >> (StructureView <$> int),
  try $ reserved "code" >> (CodeView <$> int <*> int),
  try $ reserved "sequence" >> (SequenceView <$> int),
  -- currently not parsing Example...
  try $ reserved "ensembleStatus" >> return EnsembleStatusView,
  try $ reserved "tempo" >> return TempoView,
  try $ reserved "roulette" >> (RouletteView <$> int <*> int),
  try $ reserved "audiomap" >> return AudioMapView,
  try $ reserved "stopwatch" >> (StopWatchView <$> int)
  ]

viewsParser :: Parser [View]
viewsParser = brackets $ commaSep viewParser

gridViewParser :: Parser View
gridViewParser = do
  columns <- int
  lexeme (oneOf "xX")
  whiteSpace
  rows <- int
  vs <- viewsParser
  return $ GridView columns rows vs

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
