module Estuary.Types.Terminal (Command(..),parseCommand) where

import Text.ParserCombinators.Parsec

import Estuary.Types.View
import Estuary.Types.ViewsParser

data Command =
  SetView View | -- change the current active view to a specific literal view
  StandardView | -- make the current active view the "standard" view of this Estuary build
  PresetView String | -- make the current active view a preset of this Estuary build (or Standard if not found)
  DefaultView | -- make the current active view whatever has been stored as the local/ensemble default
  ActiveView String | -- make the current active view be the named, published view
  PublishView String | -- take the current local view and publish it with a specific name
  PublishDefaultView | -- take the current local view and publish it as the default local/ensemble view
  GetView String | -- request a specific named view from the ensemble server
  ListViews | -- request the list of all named views from the ensemble server
  DeleteView String | -- delete a named view from the ensemble server
  DumpView | -- dump whatever is currently displayed
  Chat String  -- send a chat message
  deriving (Show,Eq)

parseCommand :: String -> Either ParseError Command
parseCommand = parse terminal "(unknown)"

terminal :: GenParser Char a Command
terminal = spaces >> (terminalCommand <|> chatP)

terminalCommand :: GenParser Char a Command
terminalCommand = char '!' >> choice [
  try setViewP,
  try standardViewP,
  try presetViewP,
  try defaultViewP,
  try activeViewP,
  try publishViewP,
  try publishDefaultViewP,
  try getViewP,
  try listViewsP,
  try deleteViewP,
  try dumpViewP]

setViewP = string "setview" >> spaces >> viewsParser >>= return . SetView
standardViewP = string "standardview" >> return StandardView
presetViewP = string "presetview" >> spaces >> many1 alphaNum >>= return . PresetView
defaultViewP = string "defaultview" >> return DefaultView
activeViewP = string "activeview" >> spaces >> many1 alphaNum >>= return . ActiveView
publishViewP = string "publishview" >> spaces >> many1 alphaNum >>= return . PublishView
publishDefaultViewP = string "publishdefaultview" >> return PublishDefaultView
getViewP = string "getview" >> spaces >> many1 alphaNum >>= return . GetView
listViewsP = string "listviews" >> return ListViews
deleteViewP = string "deleteview" >> spaces >> many1 alphaNum >>= return . DeleteView
chatP = many1 anyChar >>= return . Chat
dumpViewP = string "dumpview" >> return DumpView
