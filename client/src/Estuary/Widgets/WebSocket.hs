{-# LANGUAGE RecursiveDo, OverloadedStrings, JavaScriptFFI #-}

module Estuary.Widgets.WebSocket (estuaryWebSocket) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Reflex.Dom.WebSocket
import Control.Lens hiding (Context)
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson

import Estuary.Types.RenderInfo
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.Hint
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.TranslatableText
import Estuary.Widgets.W


estuaryWebSocket :: MonadWidget t m => Dynamic t RenderInfo -> Event t [Request] ->
  m (Event t Response, Event t Hint)
estuaryWebSocket rInfo toSend = mdo
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
  let wsHints = leftmost [wsOpenHint,wsErrorHint,wsCloseHint]
  let rejoinEvent = fmap pure $ attachWithMaybe maybeRejoinEnsemble (current ctx) $ ws^.webSocket_open


  -- after widget is built, query and report browser info to server
  postBuild <- getPostBuild
  let sendBrowserInfo = fmap (const [BrowserInfo userAgent]) postBuild

  -- the server responds to ClientInfo (below) with ServerInfo, which we process below
  -- by issuing events that update the context
  let serverInfos = fmapMaybe justServerInfo response
  let serverClientCounts = fmap fst serverInfos
  let serverClientCountChanges = fmap (\x c -> c { clientCount = x }) serverClientCounts
  let pingTimes = fmap snd serverInfos
  latency <- performEvent $ fmap (liftIO . (\t1 -> getCurrentTime >>= return . (flip diffUTCTime) t1)) pingTimes
  let latencyChanges = fmap (\x c -> c { serverLatency = x }) latency
  let contextChanges = mergeWith (.) [serverClientCountChanges,latencyChanges]

  -- every 5 seconds, if websocket is working, send updated ClientInfo to the server
  -- let socketIsOpen = fmap (=="connection open") status'
  let socketIsOpen = constDyn True
  pingTick <- gate (current socketIsOpen) <$> tickLossy (5::NominalDiffTime) now
  pingTickTime <- performEvent $ fmap (liftIO . const getCurrentTime) pingTick
  latencyDyn <- holdDyn 0 $ latency
  let clientInfoDyn = ClientInfo <$> fmap avgRenderLoad rInfo <*> fmap animationFPS rInfo <*> fmap animationLoad rInfo <*> latencyDyn
  let clientInfoEvent = fmap (:[]) $ attachPromptlyDynWith ($) clientInfoDyn pingTickTime

  return (response,wsHints)

maybeRejoinEnsemble :: Context -> () -> Maybe Request
maybeRejoinEnsemble ctx _
  | ensembleName (ensemble (ensembleC ctx)) == "" = Nothing
  | otherwise = Just $ RejoinEnsemble eName uName loc pwd
    where
      eName = ensembleName (ensemble (ensembleC ctx))
      uName = userHandle (ensembleC ctx)
      loc = location (ensembleC ctx)
      pwd = password (ensembleC ctx)

foreign import javascript unsafe
  "$r = location.hostname"
  getHostName :: IO Text

foreign import javascript unsafe
  "$r = location.port"
  getPort :: IO Text

foreign import javascript unsafe
  "navigator.userAgent"
  getUserAgent :: IO Text
