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
  MonitorInput (Maybe Double) | -- send audio input straight to audio output, at specified level in dB (nothing=off)
  DeleteThisEnsemble Password | -- delete the current ensemble from the server (with host password)
  DeleteEnsemble Name Password | -- delete the ensemble specified by first argument from the server (with moderator password)
  AncientTempo | -- for testing, sets active tempo to one anchored years in the past
  ShowTempo | -- for testing, displays current tempo in terminal
  SetCPS Double |
  SetBPM Double |
  InsertSound Text Text Int | -- "url" [bankName] [n]
  DeleteSound Text Int | -- [bankName] [n]
  AppendSound Text Text | -- "url" [bankName]
  ResList Text | -- "url"
  ClearResources |
  DefaultResources |
  ShowResources |
  ResetZones |
  ResetViews |
  ResetTempo |
  Reset | -- same effect as ResetZones + ResetTempo (doesn't reset views)
  SetCC Int Double | -- set a MIDI continuous-controller value (range of Double is 0-1)
  ShowCC Int | -- show a MIDI continuous-controller value in the terminal
  MaxAudioOutputs | -- query max number of audio output channels according to browser
  SetAudioOutputs Int | -- attempt to set a specific number of audio output channels
  AudioOutputs | -- query current number of output audio channels
  ListSerialPorts | -- query available WebSerial ports
  SetSerialPort Int | -- select a WebSerial port by index, and activate WebSerial output
  NoSerialPort -- disactivate WebSerial output
  deriving (Show,Eq)

parseCommand :: T.Text -> Either (Span, Text) Command
parseCommand s
  | (s' == "") || (T.head s' /= '!') = Right $ Chat s
  | otherwise = parseTerminalCommand $ removeExclamation s'
    where s' =  T.strip s

parseTerminalCommand ::  T.Text -> Either (Span, Text) Command
parseTerminalCommand s
   |all C.isSpace (T.unpack s) = Left $ (((1,1), (1,1)) , "expected command after '!' ")
   |otherwise = fmap fst $ parseAndRun terminalCommand () (T.unpack s)

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
  <|> monitorInput
  <|> deletethisensembleParser
  <|> deleteensembleParser
  <|> ancientTempoParser
  <|> showTempoParser
  <|> setCPSParser
  <|> setBPMParser
  <|> insertAudioResourceParser
  <|> deleteAudioResourceParser
  <|> appendAudioResourceParser
  <|> resListParser
  <|> clearResourcesParser
  <|> defaultResourcesParser
  <|> showResourcesParser
  <|> resetzonesParser
  <|> resetviewsParser
  <|> resettempoParser
  <|> resetParser
  <|> setCCParser
  <|> showCCParser
  <|> audioOutputsEtcParsers
  <|> serialPortsParsers
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


resetzonesParser :: H Command
resetzonesParser = ResetZones <$ (reserved "resetzones")

resetviewsParser :: H Command
resetviewsParser = ResetViews <$ (reserved "resetviews")

resettempoParser :: H Command
resettempoParser = ResetTempo <$ (reserved "resettempo")

resetParser :: H Command
resetParser = Reset <$ (reserved "reset")

setCCParser :: H Command
setCCParser =
  ((SetCC <$ reserved "setCC") <*> int <*> double) <|>
  (reserved "setCC" >> fatal "Missing arguments. !setCC expects an Int (channel) and Double (value).")

showCCParser :: H Command
showCCParser =
  ((ShowCC <$ reserved "showCC") <*> int) <|>
  (reserved "showCC" >> fatal "Missing argument. !showCC expects an Int (channel).")


audioOutputsEtcParsers :: H Command
audioOutputsEtcParsers =
  (MaxAudioOutputs <$ reserved "maxAudioOutputs") <|>
  (AudioOutputs <$ reserved "audioOutputs") <|>
  ((SetAudioOutputs <$ reserved "setAudioOutputs") <*> int) <|>
  (reserved "setAudioOutputs" >> fatal "Missing argument. !setAudioOutputs expects an Int argument.")

serialPortsParsers :: H Command
serialPortsParsers =
  (ListSerialPorts <$ reserved "listSerialPorts") <|>
  (NoSerialPort <$ reserved "noSerialPort") <|>
  ((SetSerialPort <$ reserved "setSerialPort") <*> int) <|>
  (reserved "setSerialPort" >> fatal "Missing argument. !setSerialPort expects an Int argument.")


-- select a presetview
presetView :: H Command
presetView = (presetView' <*!> (textLiteral  <|> identifierText)) <|>
             (reserved "presetview" >> fatal "Missing argument. !presetview expects a view name.")

presetView' :: H (Text -> Command)
presetView' = PresetView <$ reserved "presetview"

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
publishDefaultView = (PublishView "def" <$ reserved "publishdefaultview")

-- print the active view
activeView :: H Command
activeView = ActiveView <$ reserved  "activeview"

-- select a local view
localView :: H Command
localView = (localView' <*!> viewParser) <|>
            (reserved "localview" >> fatal "Missing argument. !localview expects a view definition.")

localView' :: H (View -> Command)
localView' = LocalView <$ reserved "localview"

-- print a list of the views
listViews :: H Command
listViews = ListViews <$ reserved "listviews"

-- dump view
dumpViewParser :: H Command
dumpViewParser = DumpView <$ reserved "dumpview"

-- start streaming
startStreaming :: H Command
startStreaming = StartStreaming <$ reserved "startstreaming"

--  streamId
streamId :: H Command
streamId = StreamId <$ reserved "streamid"

-- delay
delay :: H Command
delay = (delay' <*!> double) <|>
        (reserved "delay" >> fatal "Missing argument. !delay expects delay time (i.e. double).")

delay' :: H (Double -> Command)
delay' = Delay <$ reserved "delay"



-- monitorInput

monitorInput :: H Command
monitorInput =
  monitorInputOn <|>
  monitorInputOff <|>
  (reserved "monitorinput" >> fatal "Missing argument. !monitorinput expects off or a level in decibels.")

monitorInputOn :: H Command
monitorInputOn = do
  (_,x) <- functionApplication (reserved "monitorinput") rationalOrInteger
  return $ MonitorInput (Just $ realToFrac x)

monitorInputOff :: H Command
monitorInputOff = do
  _ <- functionApplication (reserved "monitorinput") (reserved "off")
  return $ MonitorInput Nothing

-- delete current ensemble
deletethisensembleParser :: H Command
deletethisensembleParser = (deletethisensembleParser' <*!> nameOrPassword) <|>
     (reserved "deletethisensemble" >> fatal "Missing argument. !deletethisensemble expects a host password .")


deletethisensembleParser' :: H (Text -> Command)
deletethisensembleParser' = DeleteThisEnsemble <$ reserved "deletethisensemble"

-- delete ensemble
deleteensembleParser :: H Command
deleteensembleParser = (deleteensembleParser' <*!> nameOrPassword) <|>
    (reserved "deleteensemble" >> fatal "Missing argument. !deleteensemble expects an ensemble name and a moderator password.")

deleteensembleParser' :: H (Text -> Command)
deleteensembleParser' = deleteensembleParser'' <*> nameOrPassword

deleteensembleParser'' :: H (Text -> Text -> Command)
deleteensembleParser'' = DeleteEnsemble <$ reserved "deleteensemble"

-- ancient tempo
ancientTempoParser :: H Command
ancientTempoParser = AncientTempo <$ reserved "ancienttempo"

 -- ancient tempo
showTempoParser :: H Command
showTempoParser = ShowTempo <$ reserved "showtempo"

-- set cps
setCPSParser :: H Command
setCPSParser = (setCPSParser' <*!> double) <|>
   (reserved "setcps" >> fatal "Missing argument. !setcps expects a number.")

setCPSParser' :: H (Double -> Command)
setCPSParser' = SetCPS <$ reserved "setcps"


--  set bpm

setBPMParser :: H Command
setBPMParser = (setBPMParser' <*!> double) <|>
  (reserved "setbpm" >> fatal "Missing argument. !setbpm expects a number.")

setBPMParser' :: H (Double -> Command)
setBPMParser' = SetBPM <$ reserved "setbpm"


-- insert audio resource
insertAudioResourceParser :: H Command
insertAudioResourceParser = (insertAudioResourceParser' <*!> int) <|>
  (insertAudioResourceParser''' >> fatal "Missing arguments. !insertaudio expects three parameters: url, bankName and n.")

insertAudioResourceParser' :: H (Int -> Command)
insertAudioResourceParser' = (insertAudioResourceParser'' <*!> identifierText) <|>
  (insertAudioResourceParser''' >> fatal "Missing arguments. !insertaudio expects three parameters: url, bankName and n.")

insertAudioResourceParser'' :: H (Text -> Int -> Command)
insertAudioResourceParser'' = (insertAudioResourceParser''' <*!> textLiteral) <|>
  (insertAudioResourceParser''' >> fatal "Missing arguments. !insertaudio expects three parameters: url, bankName and n.")

insertAudioResourceParser''' :: H (Text -> Text -> Int -> Command)
insertAudioResourceParser''' = reserved "insertsound" >> return InsertSound




-- delete audio resource
deleteAudioResourceParser :: H Command
deleteAudioResourceParser = (deleteAudioResourceParser' <*!> int) <|>
  (deleteAudioResourceParser'' >> fatal "Missing argument(s). !deleteaudioresource expects two parameters: bankName and n.")

deleteAudioResourceParser' :: H (Int -> Command)
deleteAudioResourceParser' = (deleteAudioResourceParser'' <*!> identifierText) <|>
  (deleteAudioResourceParser''  >> fatal "Missing argument(s). !deleteaudioresource expects two parameters: bankName and n.")

deleteAudioResourceParser'' :: H (Text -> Int -> Command)
deleteAudioResourceParser'' = DeleteSound <$ reserved "deletesound"


-- append audio source

appendAudioResourceParser :: H Command
appendAudioResourceParser = (appendAudioResourceParser' <*!> identifierText) <|>
  (appendAudioResourceParser'' >> fatal "Missing arguments. !appendaudioresource expects two parameters:  url and bankName")

appendAudioResourceParser' :: H (Text -> Command)
appendAudioResourceParser' = (appendAudioResourceParser'' <*!> textLiteral) <|>
  (appendAudioResourceParser'' >> fatal "Missing arguments. !appendaudioresource expects two parameters:  url and bankName")

appendAudioResourceParser'' :: H (Text -> Text -> Command)
appendAudioResourceParser'' = AppendSound <$ reserved "appendsound"


-- resList

resListParser :: H Command
resListParser =
  ((reserved "reslist" >> return ResList) <*> textLiteral) <|>
  (reserved "reslist" >> fatal "!reslist requires a URL argument")


-- clearResources

clearResourcesParser :: H Command
clearResourcesParser = reserved "clearresources" >> return ClearResources


-- defaultResources

defaultResourcesParser :: H Command
defaultResourcesParser = reserved "defaultresources" >> return DefaultResources


-- showResources

showResourcesParser :: H Command
showResourcesParser = reserved "showresources" >> return ShowResources

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
