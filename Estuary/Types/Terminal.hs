module Estuary.Types.Terminal (Command,parseTerminal) where

import Text.ParserCombinators.Parsec

import Estuary.Types.View

data Command =
  DefaultView |
  SetView View |
  PublishView String |
  GetView String |
  ListViews |
  DeleteView String |
  Chat String
  deriving (Show,Eq)

parseTerminal :: String -> Either ParseError Command
parseTerminal = parse terminal "(unknown)"

terminal :: GenParser Char a Command
terminal = spaces >> (terminalCommand <|> chat)

terminalCommand :: GenParser Char a Command
terminalCommand = char '!' >> choice [defaultViewP,setView,publishView,getView,listViews,deleteView]

defaultViewP = string "defaultview" >> return DefaultView
setView = string "setview" >> spaces >> viewsParser >>= return . SetView
publishView = string "publishview" >> spaces >> many1 alphaNum >>= return . PublishView
getView = string "getview" >> spaces >> many1 alphaNum >>= return . GetView
listViews = string "listviews" >> return ListViews
deleteView = string "deleteview" >> spaces >> many1 alphaNum >>= return . DeleteView
chat = many1 anyChar >>= return . Chat
