{-# LANGUAGE RecursiveDo, OverloadedStrings, JavaScriptFFI #-}

module Estuary.Widgets.WebSocket where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Text.JSON
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Either
import Data.Maybe
import Data.Text

import Estuary.Protocol.Foreign
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Context

-- an estuaryWebSocket wraps the underlying Reflex WebSocket with some parsing of the EstuaryProtocol
-- for collaborative editing. While the password is dynamic, like the Reflex WebSocket the socket address
-- isn't (so each new address requires a new instance of the widget - see resettingWebSocket below)
-- currently not working, apparently because of a bug in the old version of reflex-dom we are using
-- (see alternateWebSocket below)
-- when we refactor to new reflex we will likely reincorporate this

{- estuaryWebSocket :: MonadWidget t m => String -> Dynamic t String -> Event t EstuaryProtocol
  -> m (Event t EstuaryProtocol)
estuaryWebSocket addr pwd toSend = mdo
  let addr' = "ws://" ++ addr
  let toSend' = fmap ((:[]) . C.pack . encode) $ attachDynWith setPassword pwd toSend
  ws <- webSocket addr' $ def & webSocketConfig_send .~ toSend'
  let ws' = traceEventWith (C.unpack) $ _webSocket_recv ws
  let wsRcvd = fmap (decode . C.unpack) $ ws'
  return $ fmapMaybe isOk wsRcvd
  where
    isOk (Ok x) = Just x
    isOk _ = Just (ProtocolError "unknown protocol error")
-}

alternateWebSocket :: MonadWidget t m => EstuaryProtocolObject -> Dynamic t Context -> Event t Request ->
  m (Event t [Response], Event t (Context->Context))
alternateWebSocket obj ctx toSend = mdo
  now <- liftIO $ getCurrentTime

  performEvent_ $ fmap (liftIO . (send obj) . encode) $ leftmost [sendBrowserInfo,clientInfoEvent,toSend]
  ticks <- tickLossy (0.1::NominalDiffTime) now
  responses <- performEvent $ fmap (liftIO . (\_ -> getResponses obj)) ticks
  -- responses <- performEventAsync $ ffor ticks $ \_ cb -> liftIO (getResponses obj >>= cb) -- is this more performant???
  let responses' = fmapMaybe id $ fmap (either (const Nothing) (Just)) responses
  status <- performEvent $ fmap (liftIO . (\_ -> getStatus obj)) ticks
  status' <- holdDyn "---" status
  let wsStatusChanges = fmap (\w x -> x { wsStatus = w }) $ (updated . nubDyn) status'

  -- after widget is built, query and report browser info to server
  sendBrowserInfo <- (liftIO . BrowserInfo . getUserAgent) <$ getPostBuild

  -- every 5 seconds, if websocket is working, send updated ClientInfo to the server
  socketIsOpen <- mapDyn (=="connection open") status'
  pingTick <- gate (current socketIsOpen) <$> tickLossy (5::NominalDiffTime) now
  pingTimeTime <- performEvent $ fmap (liftIO . const getCurrentTime) pingTick
  initialTime <- liftIO getCurrentTime
  timeDyn <- holdDyn initialTime pingTickTime
  loadDyn <- fmap load ctx
  animationLoadDyn <- fmap animationLoad ctx
  latencyDyn <- holdDyn 0 $ latency
  clientInfoDyn <- ClientInfo <$> timeDyn <*> loadDyn <*> animationLoadDyn <*> latencyDyn
  let clientInfoEvent = tagDyn clientInfo pingTick

  -- the server responds to ClientInfo (above) with ServerInfo, which we process below
  -- by issuing events that update the context
  let serverInfos = fmapMaybe justServerInfo responses'
  let serverClientCounts = fmap fst serverInfos
  let serverClientCountChanges = fmap (\x c -> c { serverClientCount = x }) serverClientCounts
  let pingTimes = fmap snd serverInfos
  latency <- performEvent $ fmap (liftIO . (\t1 -> getCurrentTime >>= return . (flip diffUTCTime) t1)) pingTimes
  let latencyChanges = fmap (\x c -> c { serverLatency = x }) latency
  let contextChanges = mergeWith (.) [wsStatusChanges,serverClientCountChanges,latencyChanges]

  return (responses',contextChanges)


foreign import javascript unsafe "navigator.userAgent" getUserAgent :: IO Text
