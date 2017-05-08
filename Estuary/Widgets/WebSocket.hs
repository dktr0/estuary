{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.WebSocket where

import Reflex
import Reflex.Dom
import Text.JSON
import Estuary.Protocol
import qualified Data.ByteString.Char8 as C


-- an estuaryWebSocket wraps the underlying Reflex WebSocket with some parsing of the EstuaryProtocol
-- for collaborative editing. While the password is dynamic, like the Reflex WebSocket the socket address
-- isn't (so each new address requires a new instance of the widget - see resettingWebSocket below)

estuaryWebSocket :: MonadWidget t m => String -> Dynamic t String -> Event t EstuaryProtocol
  -> m (Event t EstuaryProtocol)
estuaryWebSocket addr pwd toSend = mdo
  let addr' = "ws://" ++ addr
  let toSend' = fmap ((:[]) . C.pack . encode) $ attachDynWith setPassword pwd toSend
  wsRcvd <- webSocket addr' $ def & webSocketConfig_send .~ toSend'
  return never -- **placeholder**


-- a resettingWebSocket is a wrapper of estuaryWebSocket above so that the webSocket address
-- is specified by event updates. A new address event causes the previous estuaryWebSocket to
-- be discarded and a new one to be created.

resettingWebSocket :: MonadWidget t m => Event t String -> Dynamic t String -> Event t EstuaryProtocol
  -> m (Event t EstuaryProtocol)
resettingWebSocket addr pwd toSend = do
  let resets = fmap (\a -> estuaryWebSocket a pwd toSend) addr
  ws <- widgetHold (return never) resets
  return $ switchPromptlyDyn ws


-- finally, a webSocketWidget includes GUI elements for setting the webSocket address and
-- password, and connects these GUI elements to a resettingWebSocket (i.e. estuaryWebSocket)

webSocketWidget :: MonadWidget t m => Event t EstuaryProtocol -> m (Event t EstuaryProtocol)
webSocketWidget toSend = do
  addr <- textInput $ def & textInputConfig_initialValue .~ "127.0.0.1:8002"
  let addr' = tagDyn (_textInput_value addr) (_textInput_keypress addr)
  pwd <- textInput $ def & textInputConfig_initialValue .~ "blah"
  let pwd' = _textInput_value pwd
  resettingWebSocket addr' pwd' toSend
