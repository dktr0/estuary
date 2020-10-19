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

import Estuary.Protocol.Peer
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleC
import Estuary.Types.Context
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Hint
import Estuary.Widgets.Editor
import Estuary.Widgets.EnsembleStatus

terminalWidget :: MonadWidget t m => Event t [Response] -> Event t [Hint] -> W t m (Event t Terminal.Command)
terminalWidget deltasDown hintsDown = divClass "terminal code-font" $ mdo
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
    let errorMsgs = fmapMaybe (either (Just . (:[]) . ("Error: " <>) . T.pack . show) (const Nothing)) parsedInput

    let hintMsgs = ffilter (/= []) $ fmap hintsToMessages hints

    -- parse responses from server in order to display log/chat messages
    let responseMsgs = fmap (Data.Maybe.mapMaybe responseToMessage) deltasDown
    let streamIdMsgs = fmap (\x -> ["new Peer id: " <> x]) streamId
    let messages = mergeWith (++) [responseMsgs,errorMsgs,hintMsgs,streamIdMsgs]
    mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
    divClass "chatMessageContainer" $ simpleList mostRecent $ \v -> divClass "chatMessage code-font primary-color" $ dynText v

    pp <- liftIO $ newPeerProtocol
    startStreamingReflex pp $ ffilter (== Terminal.StartStreaming) commands
    streamId <- peerProtocolIdReflex pp $ ffilter (== Terminal.StreamId) commands
    return commands

  divClass "ensembleStatus" $ ensembleStatusWidget

  return commands


-- these two new functions don't belong in this module
-- they should probably go in Estuary.Widgets.Estuary?

logEnsembleEvents :: MonadWidget t m => Event t [EnsembleEvent] -> W t m ()
logEnsembleEvents evs = hints $ ffilter (/=[]) $ fmap (catMaybes . fmap ensembleEventToLogMessage) evs

ensembleEventToLogMessage :: EnsembleEvent -> Maybe Hint
ensembleEventToLogMessage (TempoEvent t) = Just $ LogMessage "tempo changed"
ensembleEventToLogMessage ClearZones = Just $ LogMessage "all zones cleared"
ensembleEventToLogMessage (ViewsEvent n _) = Just $ LogMessage $ "view " <> n <> " published"
ensembleEventToLogMessage (ChatEvent c) = Just $ LogMessage $ chatSender c <> ": " <> chatText c
ensembleEventToLogMessage (JoinEvent n "" _ _) = Just $ LogMessage $ "joined ensemble " <> n <> " anonymously"
ensembleEventToLogMessage (JoinEvent n h _ _) = Just $ LogMessage $ "joined ensemble " <> n <> " as " <> h
ensembleEventToLogMessage LeaveEvent = Just $ LogMessage $ "left ensemble"
ensembleEventToLogMessage (ParticipantJoins p) = Just $ LogMessage $ name p <> " joins ensemble"
ensembleEventToLogMessage (ParticipantLeaves n) = Just $ LogMessage $ name p <> " leaves ensemble"
ensembleEventToLogMessage _ = Nothing

-- ** WORKING BELOW HERE ***
-- create a system for translating terminal commands into EnsembleEvents
-- in some cases we need to sample Dynamic information about the ensemble to do this

commandsToEnsembleEvents :: MonadWidget t m => Event t Terminal.Command -> W t m (Event t EnsembleEvent)
commandsToEnsembleEvents cmdEvents = do

  -- use tuples to put together dynamic information? hope that means it only gets sampled once though...
  let publishViewsEvents = attachDyn (current ...) $ fmapMaybe f cmdEvents
    where
      f d (Terminal.PublishView x) = ...
      f _ _ = Nothing



commandToEnsembleEvent :: (     ) -> Terminal.Command -> Maybe EnsembleEvent]
commandToEnsembleEvent (Terminal.LocalView x) = Just $ Left x
commandToEnsembleEvent (Terminal.PresetView x) = Just $ Right x
commandToEnsembleEvent _ = Nothing
commandsToEnsembleEvents :: MonadWidget t m =>
  EnsembleC -> Event t Terminal.Command -> m (Event t [EnsembleEvent])
commandsToEnsembleEvents ensC cmdEvents = do

commandToEnsembleEvent :: Terminal.Command -> Maybe EnsembleEvent
commandToEnsembleEvent (Terminal.LocalView x) = Just $ Left x
commandToEnsembleEvent (Terminal.PresetView x) = Just $ Right x
commandToEnsembleEvent (Terminal.PublishView x) =
 ... need to access current active view in order to do this...
 ViewsEvent Text View | -- a named view is published
commandToEnsembleEvent _ = Nothing

commandToStateChange :: Terminal.Command -> EnsembleC -> EnsembleC
commandToStateChange (Terminal.LocalView v) es = selectLocalView v es
commandToStateChange (Terminal.PresetView t) es = selectPresetView t es
commandToStateChange (Terminal.PublishView t) es = replaceStandardView t (activeView es) es
commandToStateChange _ es = es

-- ??? commandsToHints :: MonadWidget t m => Event t Terminal.Command -> W t m ()

-- this needs to be reworked but all the cases are commands that don't generate EnsembleEvents
-- but which do generate Hints, in all cases by various sampling operations on the EnsembleC's fields...
commandToHint es (Terminal.ActiveView) = Just $ LogMessage $ nameOfActiveView es
commandToHint es (Terminal.ListViews) = Just $ LogMessage $ showt $ listViews $ ensemble es
commandToHint es (Terminal.DumpView) = Just $ LogMessage $ dumpView (activeView es)
commandToHint _ (Terminal.Delay t) = Just $ SetGlobalDelayTime t
commandToHint es (Terminal.ShowTempo) = Just $ LogMessage $ T.pack $ show $ tempo $ ensemble es
commandToHint _ _ = Nothing

commandToEnsembleRequest :: EnsembleC -> Terminal.Command -> Maybe (IO EnsembleRequest)
commandToEnsembleRequest es (Terminal.PublishView x) = Just $ return (WriteView x (activeView es))
commandToEnsembleRequest es (Terminal.Chat x) = Just $ return (WriteChat x)
commandToEnsembleRequest es Terminal.AncientTempo = Just $ return (WriteTempo x)
  where x = Tempo { freq = 0.5, time = UTCTime (fromGregorian 2020 01 01) 0, count = 0 }
commandToEnsembleRequest es (Terminal.SetCPS x) = Just $ do
  x' <- changeTempoNow (realToFrac x) (tempo $ ensemble es)
  return (WriteTempo x')
commandToEnsembleRequest es (Terminal.SetBPM x) = Just $ do
  x' <- changeTempoNow (realToFrac x / 240) (tempo $ ensemble es)
  return (WriteTempo x')
commandToEnsembleRequest _ _ = Nothing

responseToMessage :: Response -> Maybe Text
responseToMessage (ResponseError e) = Just $ "error: " <> e
responseToMessage (ResponseOK m) = Just m
responseToMessage (EnsembleResponse (ChatRcvd c)) = Just $ showChatMessage c
responseToMessage (EnsembleResponse (ParticipantJoins x)) = Just $ name x <> " has joined the ensemble"
responseToMessage (EnsembleResponse (ParticipantLeaves n)) = Just $ n <> " has left the ensemble"
responseToMessage _ = Nothing


hintsToMessages :: [Hint] -> [Text]
hintsToMessages hs = fmapMaybe hintToMessage hs

hintToMessage :: Hint -> Maybe Text
hintToMessage (LogMessage x) = Just x
hintToMessage _ = Nothing

startStreamingReflex :: MonadWidget t m => PeerProtocol -> Event t a -> m ()
startStreamingReflex pp e = performEvent_ $ fmap (liftIO . const (startStreaming pp)) e

peerProtocolIdReflex :: MonadWidget t m => PeerProtocol -> Event t a -> m (Event t Text)
peerProtocolIdReflex pp e = performEvent $ fmap (liftIO . const (peerProtocolId pp)) e
