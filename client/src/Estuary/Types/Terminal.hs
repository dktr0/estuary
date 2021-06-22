{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Terminal (Command(..), parseCommand) where


import Language.Haskellish as LH
import qualified Language.Haskell.Exts as Exts
import qualified Text.Parsec as Pc
import qualified Text.Parsec.Text as Tx
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Maybe

-- import Text.Parsec
-- import Text.Parsec.Text
-- import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad.Identity (Identity)

import Estuary.Types.Name
import Estuary.Types.View
import Estuary.Types.View.Parser

-- type H = Haskellish ()

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
  AppendAudioResource Text Text | -- "url" [bankName]
  ResetZones |
  ResetViews |
  ResetTempo |
  Reset -- same effect as ResetZones + ResetTempo (doesn't reset views)
  deriving (Show,Eq)

parseCommand :: T.Text -> Either (Span, Text) Command
parseCommand s
  | (s' == "") || (T.head s' /= '!') = Right $ Chat s
  | otherwise = parseTerminalCommand $ removeExclamation s'
    where s' =  T.strip s

parseTerminalCommand ::  T.Text -> Either (Span, Text) Command
parseTerminalCommand s
   |all C.isSpace (T.unpack s) = Left $ (((1,1), (1,1)) , "expected command after '!' ")
   |otherwise = fmap fst $ parseAndRun terminalCommand () s

terminalCommand :: H Command
terminalCommand =
   presetView
  <|> localView
  <|> publishView
  <|> publishDefaultView
  <|> activeView
  <|> listViews
  <|> dumpViewParser
  <|> startStreaming
  <|> streamId
  <|> delay
  <|> deletethisensembleParser
  <|> deleteensembleParser
  <|> ancientTempoParser
  <|> showTempoParser
  <|> setCPSParser
  <|> setBPMParser
  <|> insertAudioResourceParser
  <|> deleteAudioResourceParser
  <|> appendAudioResourceParser
  <|> resetzonesParser
  <|> resetviewsParser
  <|> resettempoParser
  <|> resetParser
  <|> commandErrors

commandErrors :: H Command
commandErrors = wrongCommandNoArg
  <|> wrongCommandWOneTextArg
  <|> wrongCommandWOneArgDouble
  <|> wrongCommandWTwoTextArgs
  <|> wrongCommandWTextAndDouble
  <|> wrongCommandWTextTextAndDouble


-- wrong command + text Text Int
wrongCommandWTextTextAndDouble :: H Command
wrongCommandWTextTextAndDouble = do
  z <- (wrongCommandWTextTextAndDouble' <*> double)
  fatal $ "!" <> z <> " is an unrecognised command."

wrongCommandWTextTextAndDouble' :: H (Double -> Text)
wrongCommandWTextTextAndDouble' = wrongCommandWTextTextAndDouble'' <*> (identifierText <|> textLiteral)

wrongCommandWTextTextAndDouble'' :: H (Text -> Double -> Text)
wrongCommandWTextTextAndDouble'' = wrongCommandWTextTextAndDouble''' <*> (identifierText <|> textLiteral)

wrongCommandWTextTextAndDouble''' :: H (Text -> Text -> Double -> Text)
wrongCommandWTextTextAndDouble''' = do
    x <- (identifierText <|> textLiteral)
    return $ \y z w -> x

-- wrong command + text  Int
wrongCommandWTextAndDouble :: H Command
wrongCommandWTextAndDouble = do
  z <- (wrongCommandWTextAndDouble' <*> double)
  fatal $ "!" <> z <> " is an unrecognised command."

wrongCommandWTextAndDouble' :: H (Double -> Text)
wrongCommandWTextAndDouble' = wrongCommandWTextAndDouble'' <*> (identifierText <|> textLiteral)

wrongCommandWTextAndDouble'' :: H (Text -> Double -> Text)
wrongCommandWTextAndDouble'' = do
    x <- (identifierText <|> textLiteral)
    return $ \y z -> x

-- wrong command + text  text
wrongCommandWTwoTextArgs :: H Command
wrongCommandWTwoTextArgs = do
  z <- (wrongCommandWTwoTextArgs' <*> (identifierText <|> textLiteral))
  fatal $ "!" <> z <> " is an unrecognised command."

wrongCommandWTwoTextArgs' :: H (Text -> Text)
wrongCommandWTwoTextArgs' = wrongCommandWTwoTextArgs'' <*> (identifierText <|> textLiteral)

wrongCommandWTwoTextArgs'' :: H (Text -> Text -> Text)
wrongCommandWTwoTextArgs'' = do
    x <- (identifierText <|> textLiteral)
    return $ \y z -> x

-- wrong command + double or int
wrongCommandWOneArgDouble :: H Command
wrongCommandWOneArgDouble = do
  z <- (wrongCommandWOneArgDouble' <*> double)
  fatal $ "!" <> z <> " is an unrecognised command."

wrongCommandWOneArgDouble' :: H (Double -> Text)
wrongCommandWOneArgDouble' = do
    x <- (identifierText <|> textLiteral)
    return $ \y -> x

-- wrong command + text
wrongCommandWOneTextArg :: H Command
wrongCommandWOneTextArg = do
  z <- (wrongCommandWOneTextArg' <*> (identifierText <|> textLiteral))
  fatal $ "!" <> z <> " is an unrecognised command."

wrongCommandWOneTextArg' :: H (Text -> Text)
wrongCommandWOneTextArg' = do
    x <- (identifierText <|> textLiteral )
    return $ \y -> x

--
wrongCommandNoArg :: H Command
wrongCommandNoArg = do
    x <- (identifierText <|> textLiteral)
    fatal $ "!" <> x <> " is an unrecognised command."


-- reset zones
resetzonesParser :: H Command
resetzonesParser = resetzonesFunc <$ (reserved "resetzones")

resetzonesFunc :: Command
resetzonesFunc = ResetZones

--  reset views
resetviewsParser :: H Command
resetviewsParser = resetviewsFunc <$ (reserved "resetviews")

resetviewsFunc :: Command
resetviewsFunc = ResetViews

-- reset tempo
resettempoParser :: H Command
resettempoParser = resettempoFunc <$ (reserved "resettempo")

resettempoFunc :: Command
resettempoFunc = ResetTempo

-- reset
resetParser :: H Command
resetParser = resetFunc <$ (reserved "reset")

resetFunc :: Command
resetFunc = Reset

-- select a presetview
presetView :: H Command
presetView = (presetView' <*!> (textLiteral  <|> identifierText)) <|>
             (reserved "presetview" >> fatal "Missing argument. !presetview expects a view name.")

presetView' :: H (Text -> Command)
presetView' = presetViewFunc <$ reserved "presetview"

presetViewFunc :: Text -> Command
presetViewFunc x = PresetView x

  -- publish a view
publishView :: H Command
publishView = (publishView' <*!> (textLiteral <|> identifierText))  <|>
              (reserved "publishview" >> fatal "Missing argument. !publishview expects a view name.")

publishView' :: H (Text -> Command)
publishView' = publishViewFunc <$ reserved "publishview"

publishViewFunc :: Text -> Command
publishViewFunc x
  | x == "default" =  PublishView "def"
  | otherwise = PublishView x

-- publishdefaultview
publishDefaultView :: H Command
publishDefaultView = (publishDefaultViewFunc <$ reserved "publishdefaultview")

publishDefaultViewFunc :: Command
publishDefaultViewFunc = PublishView "def"

-- print the active view
activeView :: H Command
activeView = activeViewFunc <$ reserved  "activeview"

activeViewFunc :: Command
activeViewFunc = ActiveView

-- select a local view
localView :: H Command
localView = (localView' <*!> viewParser) <|>
            (reserved "localview" >> fatal "Missing argument. !localview expects a view definition.")

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
delay = (delay' <*!> double) <|>
        (reserved "delay" >> fatal "Missing argument. !delay expects delay time (i.e. double).")


delay' :: H (Double -> Command)
delay' = delayFunc <$ reserved "delay"

delayFunc :: Double -> Command
delayFunc x = Delay x

-- delete current ensemble
deletethisensembleParser :: H Command
deletethisensembleParser = (deletethisensembleParser' <*!> nameOrPassword) <|>
     (reserved "deletethisensemble" >> fatal "Missing argument. !deletethisensemble expects a host password .")


deletethisensembleParser' :: H (Text -> Command)
deletethisensembleParser' = deletethisensembleFunc <$ reserved "deletethisensemble"

deletethisensembleFunc :: Text -> Command
deletethisensembleFunc x =  DeleteThisEnsemble x

-- delete ensemble
deleteensembleParser :: H Command
deleteensembleParser = (deleteensembleParser' <*!> nameOrPassword) <|>
    (reserved "deleteensemble" >> fatal "Missing argument. !deleteensemble expects an ensemble name and a moderator password.")

deleteensembleParser' :: H (Text -> Command)
deleteensembleParser' = deleteensembleParser'' <*> nameOrPassword

deleteensembleParser'' :: H (Text -> Text -> Command)
deleteensembleParser'' = deleteensembleFunc <$ reserved "deleteensemble"

deleteensembleFunc :: Text -> Text ->  Command
deleteensembleFunc x y =  DeleteEnsemble x y

-- ancient tempo
ancientTempoParser :: H Command
ancientTempoParser = ancientTempoFunc <$ reserved "ancienttempo"

ancientTempoFunc :: Command
ancientTempoFunc = AncientTempo

 -- ancient tempo
showTempoParser :: H Command
showTempoParser = showTempoFunc <$ reserved "showtempo"

showTempoFunc :: Command
showTempoFunc = ShowTempo

-- set cps
setCPSParser :: H Command
setCPSParser = (setCPSParser' <*!> double) <|>
   (reserved "setcps" >> fatal "Missing argument. !setcps expects a number.")

setCPSParser' :: H (Double -> Command)
setCPSParser' = setCPSFunc <$ reserved "setcps"

setCPSFunc :: Double -> Command
setCPSFunc x = SetCPS x

--  set bpm

setBPMParser :: H Command
setBPMParser = (setBPMParser' <*!> double) <|>
  (reserved "setbpm" >> fatal "Missing argument. !setbpm expects a number.")


setBPMParser' :: H (Double -> Command)
setBPMParser' = setBPMFunc <$ reserved "setbpm"

setBPMFunc :: Double -> Command
setBPMFunc x = SetBPM x

-- insert audio resource
insertAudioResourceParser :: H Command
insertAudioResourceParser = (insertAudioResourceParser' <*!> int) <|>
  (reserved "insertaudioresource" >> fatal "Missing arguments. !insertaudioresource expects three parameters: url, bankName and n.")

insertAudioResourceParser' :: H (Int -> Command)
insertAudioResourceParser' = (insertAudioResourceParser'' <*!> identifierText) <|>
  (reserved "insertaudioresource" >> fatal "Missing arguments. !insertaudioresource expects three parameters: url, bankName and n.")

insertAudioResourceParser'' :: H (Text -> Int -> Command)
insertAudioResourceParser'' = (insertAudioResourceParser''' <*!> textLiteral) <|>
  (reserved "insertaudioresource" >> fatal "Missing arguments. !insertaudioresource expects three parameters: url, bankName and n.")


insertAudioResourceParser''' :: H (Text -> Text -> Int -> Command)
insertAudioResourceParser''' = insertAudioResourceFunc <$ reserved "insertaudioresource"

insertAudioResourceFunc :: Text -> Text -> Int -> Command
insertAudioResourceFunc x y z = InsertAudioResource x y z


-- delete audio resource
deleteAudioResourceParser :: H Command
deleteAudioResourceParser = (deleteAudioResourceParser' <*!> int) <|>
  (reserved "deleteaudioresource" >> fatal "Missing argument(s). !deleteaudioresource expects two parameters: bankName and n.")

deleteAudioResourceParser' :: H (Int -> Command)
deleteAudioResourceParser' = (deleteAudioResourceParser'' <*!> identifierText) <|>
  (reserved "deleteaudioresource" >> fatal "Missing argument(s). !deleteaudioresource expects two parameters: bankName and n.")

deleteAudioResourceParser'' :: H (Text -> Int -> Command)
deleteAudioResourceParser'' = deleteAudioResourceFunc <$ reserved "deleteaudioresource"

deleteAudioResourceFunc :: Text -> Int -> Command
deleteAudioResourceFunc x y = DeleteAudioResource x y

-- append audio source

appendAudioResourceParser :: H Command
appendAudioResourceParser = (appendAudioResourceParser' <*!> identifierText) <|>
  (reserved "appendaudioresource" >> fatal "Missing arguments. !appendaudioresource expects two parameters:  url and bankName")

appendAudioResourceParser' :: H (Text -> Command)
appendAudioResourceParser' = (appendAudioResourceParser'' <*!> textLiteral) <|>
  (reserved "appendaudioresource" >> fatal "Missing arguments. !appendaudioresource expects two parameters:  url and bankName")

appendAudioResourceParser'' :: H (Text -> Text -> Command)
appendAudioResourceParser'' = appendAudioResourceFunc <$ reserved "appendaudioresource"

appendAudioResourceFunc :: Text -> Text -> Command
appendAudioResourceFunc x y = AppendAudioResource x y


-- helper funcs
int :: H Int
int = fromIntegral <$> integer

double :: H Double
double = fromRational <$> rationalOrInteger

identifierTextWH :: H Text
identifierTextWH =
    identifierText <|>
  (do
   x <- int
   nonFatal $ (T.pack $ show x) <> " is not a valid view"
 ) -- <?> "expecting view"

identifierText :: H Text
identifierText = do
  s <- identifier
  return $ T.pack s

removeExclamation :: Text -> Text
removeExclamation x
  |T.head x == '!' = T.drop 1 x
  |otherwise = x
