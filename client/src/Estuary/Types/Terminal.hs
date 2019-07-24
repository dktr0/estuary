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
  SetView View | -- change the current active view to a specific literal view
  StandardView | -- make the current active view the "standard" view of this Estuary build
  PresetView Text | -- make the current active view a preset of this Estuary build (or Standard if not found)
  DefaultView | -- make the current active view whatever has been stored as the local/ensemble default
  ActiveView Text | -- make the current active view be the named, published view
  PublishView Text | -- take the current local view and publish it with a specific name
  PublishDefaultView | -- take the current local view and publish it as the default local/ensemble view
  GetView Text | -- request a specific named view from the ensemble server
  ListViews | -- request the list of all named views from the ensemble server
  DeleteView Text | -- delete a named view from the ensemble server
  DumpView | -- dump whatever is currently displayed
  Chat Text | -- send a chat message
  StartStreaming | -- start RTP streaming of Estuary audio
  StreamId -- query the id assigned to RTP streaming of Estuary audio
  deriving (Show,Eq)

parseCommand :: Text -> Either ParseError Command
parseCommand = parse terminal "(unknown)"

terminal :: Parser Command
terminal = whiteSpace >> (terminalCommand <|> chatP)

terminalCommand :: Parser Command
terminalCommand = symbol "!" >> choice [
  reserved "setview" >> viewsParser >>= return . SetView,
  reserved "standardview" >> return StandardView,
  reserved "presetview" >> identifierText >>= return . PresetView,
  reserved "defaultview" >> return DefaultView,
  reserved "activeview" >> identifierText >>= return . ActiveView,
  reserved "publishview" >> identifierText >>= return . PublishView,
  reserved "publishdefaultview" >> return PublishDefaultView,
  reserved "getview" >> identifierText >>= return . GetView,
  reserved "listviews" >> return ListViews,
  reserved "deleteview" >> identifierText >>= return . DeleteView,
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
    "setview","standardview","presetview","defaultview","activeview","publishview",
    "publishdefaultview","getview","listviews","deleteview","dumpview",
    "startstreaming","streamid"
    ],
  P.reservedOpNames = [],
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
