{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.CodeWidgetOptions where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad.Identity (Identity)


data CodeWidgetOptions =
  Nomenu | -- "nomenu"
  Noeval | -- "noeval"
  Noerrors | -- "noerrors"
  Fluxus | -- "fluxus"
  CentreAlign | -- "centre" or "center"
  RightAlign | -- "right"
  Fontsize Double -- "fontsize 2"
  deriving (Show, Eq, Generic)

instance ToJSON CodeWidgetOptions where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CodeWidgetOptions

------

-- parseCodeWidgetOptions :: Text -> Either ParseError CodeWidgetOptions
-- parseCodeWidgetOptions s = parse codeWidgetOptions "" s
--
-- codeWidgetOptions :: Parser CodeWidgetOptions
-- codeWidgetOptions = try $ choice [
--   fontSize,
--   reserved "nomenu" >> return Nomenu,
--   reserved "noeval" >> return Noeval,
--   reserved "noerrors" >> return Noerrors,
--   reserved "fluxus" >> return Fluxus,
--   (reserved "centre" <|> reserved "center") >> return CentreAlign,
--   reserved "left" >> return RightAlign
--   ]
--
-- fontSize :: Parser CodeWidgetOptions
-- fontSize = do
--   n <- double
--   return $ Fontsize n
--
-- ----------
--
-- double :: Parser Double
-- double = choice [
--   try float,
--   try $ fromIntegral <$> integer
--   ]
--
-- ----------
--
-- tokenParser :: P.GenTokenParser Text () Identity
-- tokenParser = P.makeTokenParser $ P.LanguageDef {
--   P.commentStart = "/*",
--   P.commentEnd = "*/",
--   P.commentLine = "//",
--   P.nestedComments = False,
--   P.identStart = letter <|> char '_',
--   P.identLetter = alphaNum <|> char '_',
--   P.opStart = oneOf ".",
--   P.opLetter = oneOf ".",
--   P.reservedNames = [
--     "nomenu", "noeval", "noerrors", "fluxus", "centre", "center", "left", "fontsize"
--     ],
--   P.reservedOpNames = [],
--   P.caseSensitive = False
--   }
--
-- ----------
--
-- identifier = P.identifier tokenParser
-- reserved = P.reserved tokenParser
-- operator = P.operator tokenParser
-- reservedOp = P.reservedOp tokenParser
-- charLiteral = P.charLiteral tokenParser
-- stringLiteral = P.stringLiteral tokenParser
-- natural = P.natural tokenParser
-- integer = P.integer tokenParser
-- float = P.float tokenParser
-- naturalOrFloat = P.naturalOrFloat tokenParser
-- decimal = P.decimal tokenParser
-- hexadecimal = P.hexadecimal tokenParser
-- octal = P.octal tokenParser
-- symbol = P.symbol tokenParser
-- lexeme = P.lexeme tokenParser
-- whiteSpace = P.whiteSpace tokenParser
-- parens = P.parens tokenParser
-- braces = P.braces tokenParser
-- angles = P.angles tokenParser
-- brackets = P.brackets tokenParser
-- semi = P.semi tokenParser
-- comma = P.comma tokenParser
-- colon = P.colon tokenParser
-- dot = P.dot tokenParser
-- semiSep = P.semiSep tokenParser
-- semiSep1 = P.semiSep1 tokenParser
-- commaSep = P.commaSep tokenParser
-- commaSep1 = P.commaSep1 tokenParser
