{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Terminal (Command(..),parseCommand) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad.Identity (Identity)

import Estuary.Types.Name
import Estuary.Types.View
import Estuary.Types.View.Parser

data Command =
  LocalView View | -- change the active view to a local view that is not shared/stored anywhere
  PresetView Name | -- make the current active view a named preset of current ensemble or Estuary itself
  PublishView Name | -- take the current local view and publish it with the specified name
  ActiveView | -- display name of active view if it is standard/published, otherwise report that it is a local view
  ListViews | -- display the names of all available standard/published views
  DumpView | -- display the definition of the current view, regardless of whether standard/published or local
  Chat Text | -- send a chat message
  StartStreaming | -- start RTP streaming of Estuary audio
  StreamId | -- display the id assigned to RTP streaming of Estuary audio
  Delay Double | -- delay estuary's audio output by the specified time in seconds
  DeleteThisEnsemble Password | -- delete the current ensemble from the server (with host password)
  DeleteEnsemble Name Password | -- delete the ensemble specified by first argument from the server (with moderator password)
  AncientTempo | -- for testing, sets active tempo to one anchored years in the past
  ShowTempo | -- for testing, displays current tempo in terminal
  SetCPS Double |
  SetBPM Double |
  InsertAudioResource Text Text Int | -- "url" [bankName] [n]
  DeleteAudioResource Text Int | -- [bankName] [n]
  AppendAudioResource Text Text -- "url" [bankName]
  deriving (Show,Eq)

parseCommand :: Text -> Either ParseError Command
parseCommand = parse terminal "(unknown)"

terminal :: Parser Command
terminal = do
  whiteSpace
  x <- terminalCommand <|> chatP
  eof
  return x

terminalCommand :: Parser Command
terminalCommand = symbol "!" >> choice [
  reserved "localview" >> viewParser >>= return . LocalView,
  reserved "presetview" >> identifierText >>= return . PresetView,
  reserved "publishview" >> identifierText >>= return . PublishView,
  reserved "activeview" >> return ActiveView,
  reserved "listviews" >> return ListViews,
  reserved "dumpview" >> return DumpView,
  reserved "startstreaming" >> return StartStreaming,
  reserved "streamid" >> return StreamId,
  (reserved "delay" >> return Delay) <*> double,
  (reserved "deletethisensemble" >> return DeleteThisEnsemble) <*> nameOrPassword,
  (reserved "deleteensemble" >> return DeleteEnsemble) <*> nameOrPassword <*> nameOrPassword,
  reserved "ancienttempo" >> return AncientTempo,
  reserved "showtempo" >> return ShowTempo,
  (reserved "setcps" >> return SetCPS) <*> double,
  (reserved "setbpm" >> return SetBPM) <*> double,
  (reserved "insertaudioresource" >> return InsertAudioResource) <*> textLiteral <*> identifierText <*> int,
  (reserved "deleteaudioresource" >> return DeleteAudioResource) <*> identifierText <*> int,
  (reserved "appendaudioresource" >> return AppendAudioResource) <*> textLiteral <*> identifierText
  ]

identifierText :: Parser Text
identifierText = T.pack <$> identifier

textLiteral :: Parser Text
textLiteral = T.pack <$> stringLiteral

chatP = many1 anyChar >>= return . Chat . T.pack

int :: Parser Int
int = choice [
  symbol "-" >> integer >>= (return . (* (-1)) . fromIntegral),
  try $ fromIntegral <$> integer
  ]

double :: Parser Double
double = choice [
  symbol "-" >> double >>= return . (* (-1)),
  try float,
  try $ fromIntegral <$> integer
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
    "localview","presetview","publishview","activeview","listviews",
    "dumpview","startstreaming","streamid","delay","deletethisensemble",
    "deleteensemble","ancienttempo","showtempo","setcps","setbpm",
    "insertaudioresource","deleteaudioresource","appendaudioresource"
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
