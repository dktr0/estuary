module Estuary.Types.ViewsParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.List (intercalate)

import Estuary.Types.View

dumpView :: View -> String
dumpView (Views xs) = intercalate " " $ fmap dumpView xs
dumpView (ViewDiv css v) = "{ " ++ css ++ " " ++ dumpView v ++ " }"
dumpView (StructureView x) = "structure:" ++ show x
dumpView (LabelView x) = "label:" ++ show x
dumpView (TidalTextView x y) = "textView:" ++ show x ++ " " ++ show y
dumpView (EvaluableTextView x) = "evaluable:" ++ show x
dumpView (SvgDisplayView) = "svgDisplayView:"

viewsParser :: GenParser Char a View
viewsParser = do
  spaces
  many1 viewParser >>= return . Views

viewParser :: GenParser Char a View
viewParser = do
  v <- choice [
    try viewDiv,
    try labelView,
    try structureView,
    try tidalTextView,
    try evaluableTextView,
    svgDisplayView]
  spaces
  return v

viewDiv = between (char '{') (char '}') $ do
  spaces
  cssClass <- many1 alphaNum
  skipMany1 space
  vs <- viewsParser
  spaces
  return $ ViewDiv cssClass vs

labelView = string "label:" >> (read <$> many1 digit) >>= return . LabelView
structureView = string "structure:" >> (read <$> many1 digit) >>= return . StructureView
evaluableTextView = string "evaluable:" >> (read <$> many1 digit) >>= return . EvaluableTextView
tidalTextView = do
  string "textView:"
  x <- read <$> many1 digit
  skipMany1 space
  y <- read <$> many1 digit
  return $ TidalTextView x y
svgDisplayView = string "svgDisplayView:" >> return SvgDisplayView
