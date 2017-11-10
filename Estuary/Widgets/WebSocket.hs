{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.WebSocket where

import Reflex
import Reflex.Dom
import Text.JSON
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Either
import Data.Maybe

import Estuary.Protocol.Foreign
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse

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


alternateWebSocket :: MonadWidget t m => EstuaryProtocolObject -> UTCTime -> Event t ServerRequest ->
  m (Event t [ServerResponse],Dynamic t String)
alternateWebSocket obj now toSend = do
  performEvent_ $ fmap (liftIO . (send obj) . encode) toSend
  ticks <- tickLossy (0.1::NominalDiffTime) now
  responses <- performEvent $ fmap (liftIO . (\_ -> getResponses obj)) ticks
  let responses' = fmapMaybe id $ fmap (either (const Nothing) (Just)) responses
  status <- performEvent $ fmap (liftIO . (\_ -> getStatus obj)) ticks
  status' <- holdDyn "---" status
  return (responses',status')


chatWidget :: MonadWidget t m => String -> Event t [ServerResponse] -> m (Event t ServerRequest)
chatWidget space deltasDown = mdo
  let attrs = constDyn ("class" =: "webSocketTextInputs")
  text "Name:"
  nameInput <- textInput $ def & textInputConfig_attributes .~ attrs
  text "Chat:"
  let resetText = fmap (const "") send''
  chatInput <- textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
  send <- divClass "webSocketButtons" $ button "Send"
  let send' = fmap (const ()) $ ffilter (==13) $ _textInput_keypress chatInput
  let send'' = leftmost [send,send']
  let toSend = tag (current $ _textInput_value chatInput) send''
  let deltasUp = attachDynWith (\name msg -> EnsembleRequest (Sited space (SendChat name msg))) (_textInput_value nameInput) toSend
  let chatsOnly = fmap (justChats . justSited space .  justEnsembleResponses) deltasDown
  mostRecent <- foldDyn (\a b -> take 8 $ (reverse a) ++ b) [] chatsOnly
  formatted <- mapDyn (fmap (\(n,m) -> n ++ ": " ++ m)) mostRecent
  simpleList formatted chatMsg
  return deltasUp
  where chatMsg v = divClass "chatMessage" $ dynText v
