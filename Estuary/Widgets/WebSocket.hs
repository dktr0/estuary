{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.WebSocket where

import Reflex
import Reflex.Dom
import Text.JSON
import Estuary.Protocol
import qualified Data.ByteString.Char8 as C


estuaryWebSocket :: MonadWidget t m => String -> Dynamic t String -> Event t EstuaryProtocol
  -> m (Event t EstuaryProtocol)
estuaryWebSocket addr pwd toSend = mdo
  let addr' = "ws://" ++ addr
  let cpwd = attachDynWith setPassword pwd toSend
  let json = fmap encode cpwd
  let bs = fmap C.pack json
  let wsSend = fmap (:[]) bs
  wsRcvd <- webSocket addr' $ def & webSocketConfig_send .~ wsSend
  return never -- **placeholder**


resettableWebSocket :: MonadWidget t m => Event t String -> Dynamic t String -> Event t EstuaryProtocol
  -> m (Event t EstuaryProtocol)
resettableWebSocket addr pwd toSend = mdo
  let i = estuaryWebSocket "127.0.0.1:8002" pwd toSend
  let resets = fmap (\a -> estuaryWebSocket a pwd toSend) addr
  ws <- widgetHold i resets -- :: m (Dynamic t (Event t EstuaryProtocol))
  return $ switchPromptlyDyn ws
