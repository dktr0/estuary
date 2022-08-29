{-# LANGUAGE RecursiveDo, OverloadedStrings, JavaScriptFFI, FlexibleContexts #-}

module Estuary.Widgets.WebSocket (estuaryWebSocket) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Reflex.Dom.WebSocket
import Control.Lens hiding (Context)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Time
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.Hint
import Estuary.Types.TranslatableText
import Estuary.Types.ServerInfo
import Estuary.Widgets.W hiding (clientCount,serverLatency)


estuaryWebSocket :: (Reflex t, MonadIO m, MonadFix m, PostBuild t m, PerformEvent t m, MonadIO (Performable m), TriggerEvent t m, MonadHold t m) => Event t [Request] -> W t m (Event t Response, Dynamic t ServerInfo)
estuaryWebSocket toSend = mdo
  hostName <- liftIO $ getHostName
  port <- liftIO $ getPort
  userAgent <- liftIO $ getUserAgent
  now <- liftIO $ getCurrentTime

  -- the web socket itself
  let url = "wss://" <> hostName <> ":" <> port
  let requestsToSend = mergeWith (++) [sendBrowserInfo,toSend,clientInfoEvent,rejoinEvent]
  let config = def & webSocketConfig_send .~ requestsToSend & webSocketConfig_reconnect .~ True
  ws <- jsonWebSocket url config
  let response = fmapMaybe id $ ws^.webSocket_recv

  -- respond to websocket open, error, and close events
  let wsOpenHint = LogMessage <$> (english "websocket opened" <$ (ws^.webSocket_open))
  let wsErrorHint = LogMessage <$> (english "websocket error" <$ ws^.webSocket_error)
  let wsCloseHint = LogMessage <$> (english "websocket closed" <$ ws^.webSocket_close)
  hint $ leftmost [wsOpenHint,wsErrorHint,wsCloseHint]

  -- attempt to rejoin an ensemble if web socket connection re-opens
  eName <- ensembleName
  uName <- userHandle
  loc <- location
  pwd <- password
  let maybeRejoinEnsemble = ffilter (/= "") $ tag (current eName) $ ws^.webSocket_open
  let rejoinMessage = RejoinEnsemble <$> eName <*> uName <*> loc <*> pwd
  let rejoinEvent = fmap pure $ tag (current rejoinMessage) maybeRejoinEnsemble

  -- after widget is built, query and report browser info to server
  postBuild <- getPostBuild
  let sendBrowserInfo = fmap (const [BrowserInfo userAgent]) postBuild

  sInfo <- trackServerInfo response

  -- every 5 seconds, if websocket is working, send updated ClientInfo to the server
  -- let socketIsOpen = fmap (=="connection open") status'
  let socketIsOpen = constDyn True
  pingTick <- gate (current socketIsOpen) <$> tickLossy (5::NominalDiffTime) now
  pingTickTime <- performEvent $ fmap (liftIO . const getCurrentTime) pingTick
  let latencyDyn = fmap serverLatency sInfo
  arl <- avgRenderLoad
  afps <- animationFPS
  aload <- animationLoad
  let clientInfoDyn = ClientInfo <$> arl <*> afps <*> aload <*> latencyDyn
  let clientInfoEvent = fmap (:[]) $ attachPromptlyDynWith ($) clientInfoDyn pingTickTime

  return (response,sInfo)


trackServerInfo :: (Monad m, Reflex t, MonadHold t m, PerformEvent t m, MonadFix m, MonadIO (Performable m)) => Event t Response -> m (Dynamic t ServerInfo)
trackServerInfo responseDown = do
  let serverInfoEv = fmapMaybe justServerInfo responseDown -- Event t (Maybe Int,UTCTime)
  let serverClientCounts = fmap fst serverInfoEv
  let serverClientCountChanges = fmap (\x s -> s { clientCount = x }) serverClientCounts
  let pingTimes = fmap snd serverInfoEv
  latency <- performEvent $ fmap (liftIO . (\t1 -> getCurrentTime >>= return . (flip diffUTCTime) t1)) pingTimes
  let latencyChanges = fmap (\x c -> c { serverLatency = x }) latency
  let serverInfoChanges = mergeWith (.) [serverClientCountChanges,latencyChanges]
  foldDyn ($) emptyServerInfo serverInfoChanges


maybeRejoinEnsemble :: (Text,Text,Text,Text) -> () -> Maybe Request
maybeRejoinEnsemble (eName,uName,loc,pwd) _
  | eName == "" = Nothing
  | otherwise = Just $ RejoinEnsemble eName uName loc pwd

foreign import javascript unsafe
  "$r = location.hostname"
  getHostName :: IO Text

foreign import javascript unsafe
  "$r = location.port"
  getPort :: IO Text

foreign import javascript unsafe
  "navigator.userAgent"
  getUserAgent :: IO Text
