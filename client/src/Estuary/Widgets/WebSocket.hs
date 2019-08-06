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

import Estuary.Types.Context
import Estuary.Types.RenderInfo
import Estuary.Types.Request
import Estuary.Types.Response


estuaryWebSocket :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Event t [Request] ->
  m (Event t Response, Event t ContextChange)
estuaryWebSocket ctx rInfo toSend = mdo
  hostName <- liftIO $ getHostName
  port <- liftIO $ getPort
  let url = "wss://" <> hostName <> ":" <> port
  let requestsToSend = mergeWith (++) [sendBrowserInfo,toSend,clientInfoEvent]
  let config = def & webSocketConfig_send .~ requestsToSend & webSocketConfig_reconnect .~ True
  ws <- jsonWebSocket url config
  let response = fmapMaybe id $ ws^.webSocket_recv

--  status <- performEvent $ fmap (liftIO . (\_ -> getStatus obj)) ticks
--  status' <- holdDyn "---" status
--  status'' <- holdUniqDyn status'
--  let wsStatusChanges = fmap (\w x -> x { wsStatus = w }) $ updated status''

  -- after widget is built, query and report browser info to server
  postBuild <- getPostBuild
  userAgent <- performEvent $ fmap (liftIO . const getUserAgent) postBuild
  let sendBrowserInfo = fmap ((:[]) . BrowserInfo) userAgent

  -- every 5 seconds, if websocket is working, send updated ClientInfo to the server
  -- let socketIsOpen = fmap (=="connection open") status'
  let socketIsOpen = constDyn True
  now <- liftIO $ getCurrentTime
  pingTick <- gate (current socketIsOpen) <$> tickLossy (5::NominalDiffTime) now
  pingTickTime <- performEvent $ fmap (liftIO . const getCurrentTime) pingTick
  let clientInfoWithPingTime = fmap ClientInfo pingTickTime
  let loadDyn = fmap avgRenderLoad rInfo
  let animationLoadDyn = fmap avgAnimationLoad rInfo
  latencyDyn <- holdDyn 0 $ latency
  let loadAnimationAndLatency = (\x y z -> (x,y,z)) <$> loadDyn <*> animationLoadDyn <*> latencyDyn
  let clientInfoEvent = fmap (:[]) $ attachPromptlyDynWith (\(x,y,z) w -> w x y z) loadAnimationAndLatency clientInfoWithPingTime

  -- the server responds to ClientInfo (above) with ServerInfo, which we process below
  -- by issuing events that update the context
  let serverInfos = fmapMaybe justServerInfo response
  let serverClientCounts = fmap fst serverInfos
  let serverClientCountChanges = fmap (\x c -> c { clientCount = x }) serverClientCounts
  let pingTimes = fmap snd serverInfos
  latency <- performEvent $ fmap (liftIO . (\t1 -> getCurrentTime >>= return . (flip diffUTCTime) t1)) pingTimes
  let latencyChanges = fmap (\x c -> c { serverLatency = x }) latency
  let contextChanges = mergeWith (.) [serverClientCountChanges,latencyChanges]

  return (response,contextChanges)


foreign import javascript unsafe
  "$r = location.hostname"
  getHostName :: IO Text

foreign import javascript unsafe
  "$r = location.port"
  getPort :: IO Text

foreign import javascript unsafe
  "navigator.userAgent"
  getUserAgent :: IO Text
