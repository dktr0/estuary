{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Terminal (terminalWidget) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Maybe
import Data.Map.Strict (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import TextShow
import Sound.MusicW.AudioContext

import Estuary.Protocol.Peer
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleC
import Estuary.Widgets.Reflex
import Estuary.Render.DynamicsMode
import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Hint
import Estuary.Widgets.W
import Estuary.Widgets.EnsembleStatus
import Estuary.Types.TranslatableText
import Estuary.Render.R
import Estuary.Render.MainBus
import qualified Estuary.Render.WebSerial as WebSerial

import Estuary.Types.Language

terminalWidget :: MonadWidget t m => Event t [Response] -> Event t [Hint] -> W t m (Event t Terminal.Command)
terminalWidget deltasDown hints = divClass "terminal code-font" $ mdo
  commands <- divClass "chat" $ mdo
    (inputWidget) <- divClass "terminalHeader code-font primary-color" $ do
      divClass "webSocketButtons" $ term Term.TerminalChat >>= dynText
      let resetText = fmap (const "") terminalInput
      let attrs = constDyn $ fromList [("class","primary-color code-font"),("style","width: 100%")]
      inputWidget' <- divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
      return (inputWidget')
    let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
    let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [enterPressed]
    let parsedInput = fmap Terminal.parseCommand terminalInput
    let commands = fmapMaybe (either (const Nothing) Just) parsedInput
    let errorMsgs = fmap (fmap english) $ fmapMaybe (either (Just . (:[]) . ("Error: " <>) . T.pack . show) (const Nothing)) parsedInput -- [Event t Text]
    let hintMsgs' = fmap hintsToMessages $ mergeWith (++) [hints,fmap pure commandHints] -- Event t [TranslatableText]
    let hintMsgs = ffilter (/= []) hintMsgs' --
    -- parse responses from server in order to display log/chat messages
    let responseMsgs = fmap (\x -> fmap english (Data.Maybe.mapMaybe responseToMessage x)) deltasDown -- [Event t Text]
    let streamIdMsgs = fmap (\x -> fmap english ["new Peer id: " <> x]) streamId -- Event t [TranslatableText]
    let messages = mergeWith (++)  [responseMsgs, errorMsgs, hintMsgs, streamIdMsgs]

    mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
    divClass "chatMessageContainer" $ simpleList mostRecent $ \v -> do
      v' <- dynTranslatableText v -- W t m (Dynamic t Text)
      divClass "chatMessage code-font primary-color" $ dynText v' -- m()

    pp <- liftIO $ newPeerProtocol
    irc <- renderEnvironment
    commandHints <- performCommands pp irc commands
    streamId <- peerProtocolIdReflex pp $ ffilter (== Terminal.StreamId) commands
    return commands

  divClass "ensembleStatus" $ ensembleStatusWidget

  return commands


hintsToMessages :: [Hint] -> [TranslatableText] -- [TranslatableText]
hintsToMessages hs = fmapMaybe hintToMessage hs -- [x ..]

hintToMessage :: Hint -> Maybe TranslatableText --TranslatableText-- Map Language Text
hintToMessage (LogMessage x) = Just x -- translatableText $ Data.Map.fromList [(English, x)]
hintToMessage _ = Nothing

performCommands :: MonadWidget t m => PeerProtocol -> RenderEnvironment -> Event t Terminal.Command -> m (Event t Hint)
performCommands pp rEnv x = do
  y <- performEvent $ fmap (liftIO . doCommands pp rEnv) x
  return $ fmap (LogMessage . english) $ fmapMaybe id y

doCommands :: PeerProtocol -> RenderEnvironment -> Terminal.Command -> IO (Maybe Text)
doCommands _ irc (Terminal.MonitorInput x) = changeMonitorInput (mainBus irc) x >> return Nothing
doCommands pp _ Terminal.StartStreaming = startStreaming pp >> return Nothing
doCommands _ irc (Terminal.SetCC n v) = setCC n v irc >> return Nothing
doCommands _ irc (Terminal.ShowCC n) = do
  x <- getCC n irc -- :: Maybe Double
  return $ Just $ case x of
    Just x' -> "CC" <> showt n <> " = " <> showt x'
    Nothing -> "CC" <> showt n <> " not set"
doCommands _ _ Terminal.MaxAudioOutputs = liftAudioIO $ do
  n <- maxChannelCount
  return $ Just $ "maxAudioOutputs = " <> showt n
doCommands _ _ Terminal.AudioOutputs = liftAudioIO $ do
  n <- channelCount
  return $ Just $ "audioOutputs = " <> showt n
doCommands _ irc (Terminal.SetAudioOutputs n) = do
  setAudioOutputs (webDirt irc) (mainBus irc) n
  n' <- liftAudioIO $ channelCount
  return $ Just $ "audioOutputs = " <> showt n'
doCommands _ rEnv Terminal.ListSerialPorts = do
  portMap <- WebSerial.listPorts (webSerial rEnv)
  pure $ Just $ T.pack $ show portMap
doCommands _ rEnv (Terminal.SetSerialPort n) = WebSerial.setActivePort (webSerial rEnv) n >> pure (Just "serial port set")
doCommands _ rEnv Terminal.NoSerialPort = WebSerial.setNoActivePort (webSerial rEnv) >> pure (Just "serial port disactivated")
doCommands _ _ _ = return Nothing


peerProtocolIdReflex :: MonadWidget t m => PeerProtocol -> Event t a -> m (Event t Text)
peerProtocolIdReflex pp e = performEvent $ fmap (liftIO . const (peerProtocolId pp)) e
