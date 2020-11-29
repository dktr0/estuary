{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Terminal (Command(..),parseCommand) where


import Language.Haskellish as LH
import qualified Language.Haskell.Exts as Exts
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C

-- import Text.Parsec
-- import Text.Parsec.Text
-- import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad.Identity (Identity)

import Estuary.Types.Name
import Estuary.Types.View
import Estuary.Types.View.Parser

type H = Haskellish ()

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


parseCommand ::  T.Text -> Either String Command -- Text -> Either ParseError Command
parseCommand s = (f . Exts.parseExp)  $ removeExclamation $ T.unpack s
    where
      f (Exts.ParseOk x) = fmap fst $ runHaskellish terminal () x -- Either String (a, st) -- monad -- Either String a
      f (Exts.ParseFailed l s) = Left s


terminal :: H Command
terminal = terminalCommand

-- chatP = many1 anyChar >>= return . Chat . T.pack

chatParser :: H Command
chatParser = chatFunc <$ (reserved "test")

chatFunc :: Command
chatFunc = Chat "hola"

removeExclamation :: String -> String
removeExclamation x
  |head x == '!' = drop 1 x
  |otherwise = x

terminalCommand :: H Command
terminalCommand =
      localView
  <|> presetView
  <|> publishView
  <|> activeView
  <|> listViews
  <|> dumpViewParser
  <|> startStreaming
  <|> streamId
  <|> delay
  -- (reserved "deletethisensemble" >> return DeleteThisEnsemble) <*> nameOrPassword,
  -- <|> deletethisensembleParser
  -- (reserved "deleteensemble" >> return DeleteEnsemble) <*> nameOrPassword <*> nameOrPassword,
  -- <|> deleteensembleParser
  <|> ancientTempoParser
  <|> showTempoParser
  <|> setCPSParser
  <|> setBPMParser
  <|> insertAudioResourceParser
  <|> deleteAudioResourceParser
  <|> appendAudioResourceParser

-- select a presetview
presetView :: H Command
presetView = presetView' <*> identifierText

presetView' :: H (Text -> Command)
presetView' = presetViewFunc <$ (reserved "presetview")

presetViewFunc :: Text -> Command
presetViewFunc x = PresetView x

identifierText :: H Text
identifierText = do
  s <- identifier
  return $ T.pack s

  -- publish a view

publishView :: H Command
publishView = publishView' <*> identifierText

publishView' :: H (Text -> Command)
publishView' = publishViewFunc <$ reserved "publishview"

publishViewFunc :: Text -> Command
publishViewFunc x = PublishView x

-- print the active view
activeView :: H Command
activeView = activeViewFunc <$ reserved  "activeview"

activeViewFunc :: Command
activeViewFunc = ActiveView

-- select a local view
localView :: H Command
localView = localView' <*> viewParser

localView' :: H (View -> Command)
localView' = localViewFunc <$ reserved "localview"

localViewFunc :: View -> Command
localViewFunc vx = LocalView vx

-- print a list of the views
listViews :: H Command
listViews = listViewsFunc <$ reserved "listviews"

listViewsFunc :: Command
listViewsFunc = ListViews

-- dump view
dumpViewParser :: H Command
dumpViewParser = dumpViewFunc <$ reserved "dumpview"

dumpViewFunc :: Command
dumpViewFunc = DumpView

-- start streaming
startStreaming :: H Command
startStreaming = startStreamingFunc <$ reserved "startstreaming"

startStreamingFunc :: Command
startStreamingFunc = StartStreaming

--  streamId
streamId :: H Command
streamId = streamIdFunc <$ reserved "streamid"

streamIdFunc :: Command
streamIdFunc = StreamId

-- delay
delay :: H Command
delay = delay' <*> double

delay' :: H (Double -> Command)
delay' = delayFunc <$ reserved "delay"

delayFunc :: Double -> Command
delayFunc x = Delay x

-- delete current ensemble
-- deletethisensembleParser :: H Command
-- deletethisensembleParser = deletethisensembleParser' <*> nameOrPassword

-- deletethisensembleParser' :: H (Text -> Command)
-- deletethisensembleParser' = deletethisensembleFunc <$ reserved "deletethisensemble"

-- deletethisensembleFunc :: Text -> Command
-- deletethisensembleFunc x =  DeleteThisEnsemble x

-- delete ensemble
-- deleteensembleParser :: H Command
-- deleteensembleParser = deleteensembleParser' <*> nameOrPassword
--
-- deleteensembleParser' :: H (Text -> Command)
-- deleteensembleParser' = deleteensembleParser'' <*> nameOrPassword
--
-- deleteensembleParser'' :: H (Text -> Text -> Command)
-- deleteensembleParser'' = deleteensembleFunc <$ reserved "deleteensemble"
--
-- deleteensembleFunc :: Text -> Text ->  Command
-- deleteensembleFunc x y =  DeleteEnsemble x y

-- ancient tempo
ancientTempoParser :: H Command
ancientTempoParser = ancientTempoFunc <$ reserved "ancienttempo"

ancientTempoFunc :: Command
ancientTempoFunc = AncientTempo

 -- ancient tempo
showTempoParser :: H Command
showTempoParser = showTempoFunc <$ reserved "deshowtempolay"

showTempoFunc :: Command
showTempoFunc = ShowTempo

-- set cps

setCPSParser :: H Command
setCPSParser = setCPSParser' <*> double

setCPSParser' :: H (Double -> Command)
setCPSParser' = setCPSFunc <$ reserved "setcps"

setCPSFunc :: Double -> Command
setCPSFunc x = SetCPS x

--  set bpm

setBPMParser :: H Command
setBPMParser = setBPMParser' <*> double

setBPMParser' :: H (Double -> Command)
setBPMParser' = setBPMFunc <$ reserved "setbpm"

setBPMFunc :: Double -> Command
setBPMFunc x = SetBPM x

-- insert audio resource
insertAudioResourceParser :: H Command
insertAudioResourceParser = insertAudioResourceParser' <*> int

insertAudioResourceParser' :: H (Int -> Command)
insertAudioResourceParser' = insertAudioResourceParser'' <*> identifierText

insertAudioResourceParser'' :: H (Text -> Int -> Command)
insertAudioResourceParser'' = insertAudioResourceParser''' <*> textLiteral

insertAudioResourceParser''' :: H (Text -> Text -> Int -> Command)
insertAudioResourceParser''' = insertAudioResourceFunc <$ reserved "insertaudioresource"

insertAudioResourceFunc :: Text -> Text -> Int -> Command
insertAudioResourceFunc x y z = InsertAudioResource x y z


--
-- delete audio resource

deleteAudioResourceParser :: H Command
deleteAudioResourceParser = deleteAudioResourceParser' <*> int

deleteAudioResourceParser' :: H (Int -> Command)
deleteAudioResourceParser' = deleteAudioResourceParser'' <*> identifierText

deleteAudioResourceParser'' :: H (Text -> Int -> Command)
deleteAudioResourceParser'' = deleteAudioResourceFunc <$ reserved "deleteaudioresource"

deleteAudioResourceFunc :: Text -> Int -> Command
deleteAudioResourceFunc x y = DeleteAudioResource x y

-- append audio source

appendAudioResourceParser :: H Command
appendAudioResourceParser = appendAudioResourceParser' <*> identifierText

appendAudioResourceParser' :: H (Text -> Command)
appendAudioResourceParser' = appendAudioResourceParser'' <*> textLiteral

appendAudioResourceParser'' :: H (Text -> Text -> Command)
appendAudioResourceParser'' = appendAudioResourceFunc <$ reserved "appendaudioresource"

appendAudioResourceFunc :: Text -> Text -> Command
appendAudioResourceFunc x y = AppendAudioResource x y




int :: H Int
int = fromIntegral <$> integer

double :: H Double
double = fromRational <$> rationalOrInteger

textLiteral :: H Text
textLiteral = do
  s <- string
  return $ T.pack s
