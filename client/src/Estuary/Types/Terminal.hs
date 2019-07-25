{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Terminal (Command(..),parseCommand) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad.Identity (Identity)

import Estuary.Types.View
import Estuary.Types.ViewsParser

data Command =
  LocalView View | -- change the active view to a local view that is not shared/stored anywhere
  PresetView Text | -- make the current active view a named preset of current ensemble or Estuary itself
  PublishView Text | -- take the current local view and publish it with the specified name
  ActiveView | -- display name of active view if it is standard/published, otherwise report that it is a local view
  ListViews | -- display the names of all available standard/published views
  DumpView | -- display the definition of the current view, regardless of whether standard/published or local
  Chat Text | -- send a chat message
  StartStreaming | -- start RTP streaming of Estuary audio
  StreamId -- display the id assigned to RTP streaming of Estuary audio
  deriving (Show,Eq)

parseCommand :: Text -> Either ParseError Command
parseCommand = parse terminal "(unknown)"

terminal :: Parser Command
terminal = whiteSpace >> (terminalCommand <|> chatP)

terminalCommand :: Parser Command
terminalCommand = symbol "!" >> choice [
  reserved "localview" >> viewsParser >>= return . LocalView,
  reserved "presetview" >> identifierText >>= return . PresetView,
  reserved "publishview" >> identifierText >>= return . PublishView,
  reserved "activeview" >> return ActiveView,
  reserved "listviews" >> return ListViews,
  reserved "dumpview" >> return DumpView,
  reserved "startstreaming" >> return StartStreaming,
  reserved "streamid" >> return StreamId
  ]

identifierText :: Parser Text
identifierText = T.pack <$> identifier

chatP = many1 anyChar >>= return . Chat . T.pack

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
    "localview","presetview","publishview","activeview","listviews",
    "dumpview","startstreaming","streamid"
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
