{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.WebSocket where

import Reflex
import Reflex.Dom
import Text.JSON
import Estuary.Protocol.JSON
import qualified Data.ByteString.Char8 as C
import Estuary.Protocol.Foreign
import Control.Monad.IO.Class (liftIO)
import Data.Time


-- an estuaryWebSocket wraps the underlying Reflex WebSocket with some parsing of the EstuaryProtocol
-- for collaborative editing. While the password is dynamic, like the Reflex WebSocket the socket address
-- isn't (so each new address requires a new instance of the widget - see resettingWebSocket below)
-- currently not working, apparently because of a bug in the old version of reflex-dom we are using
-- (see alternateWebSocket below)

estuaryWebSocket :: MonadWidget t m => String -> Dynamic t String -> Event t EstuaryProtocol
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


-- a resettingWebSocket is a wrapper of estuaryWebSocket above so that the webSocket address
-- is specified by event updates. A new address event causes the previous estuaryWebSocket to
-- be discarded and a new one to be created. But we're not using this - instead we use 
-- alternateWebSocket below (which works with old reflex via our javascript ffi workaround)

resettingWebSocket :: MonadWidget t m => Event t String -> Dynamic t String -> Event t EstuaryProtocol
  -> m (Event t EstuaryProtocol)
resettingWebSocket addr pwd toSend = do
  let resets = fmap (\a -> estuaryWebSocket a pwd toSend) addr
  ws <- widgetHold (return never) resets
  return $ switchPromptlyDyn ws

alternateWebSocket :: MonadWidget t m => EstuaryProtocolObject -> UTCTime -> Event t String -> Dynamic t String -> Event t EstuaryProtocol -> m (Event t [EstuaryProtocol])
alternateWebSocket obj now addr pwd toSend = do
  ticks <- tickLossy (0.1::NominalDiffTime) now
  let addr' = fmap ("ws://" ++) addr
  performEvent_ $ fmap (liftIO . (setUrl obj)) addr'
  let toSend' = attachDynWith setPassword pwd toSend
  let toSend'' = fmap (encode) toSend'
  performEvent_ $ fmap (liftIO . (send obj)) toSend''
  performEvent $ fmap (liftIO . (\_ -> getEdits obj)) ticks
  
-- finally, a webSocketWidget includes GUI elements for setting the webSocket address and
-- password, and the chat interface, and connects these GUI elements to a resettingWebSocket (i.e. estuaryWebSocket)

webSocketWidget :: MonadWidget t m => EstuaryProtocolObject -> UTCTime -> Event t EstuaryProtocol -> m (Event t [EstuaryProtocol])
webSocketWidget obj now toSend = divClass "webSocketWidget" $ mdo
  (addr',pwd') <- divClass "webSocketWidgetLine1" $ do
    text "WebSocket Address:"
    addrInput <- textInput $ def & textInputConfig_initialValue .~ ""
    cButton <- button "Connect"
    statusText <- holdDyn "Status: " never
    dynText statusText
    let addr= tagDyn (_textInput_value addrInput) cButton
    text "Password:"
    pwdInput <- textInput $ def & textInputConfig_initialValue .~ ""
    let pwd = _textInput_value pwdInput
    return (addr,pwd)
  chatSend <- chatWidget deltasDown 
  let toSend' = leftmost [toSend,chatSend]
  deltasDown <- alternateWebSocket obj now addr' pwd' toSend'
  return $ deltasDown

chatWidget :: MonadWidget t m => Event t [EstuaryProtocol] -> m (Event t EstuaryProtocol)
chatWidget deltasDown = divClass "chatWidget" $ mdo
  text "Name:"
  nameInput <- textInput $ def 
  text "Chat:"
  chatInput <- textInput $ def & textInputConfig_setValue .~ resetText
  send <- button "Send"
  let send' = fmap (const ()) $ ffilter (==13) $ _textInput_keypress chatInput
  let send'' = leftmost [send,send']
  let toSend = tag (current $ _textInput_value chatInput) send''
  let resetText = fmap (const "") send''
  let deltasUp = attachDynWith (Chat "") (_textInput_value nameInput) toSend
  let chatsOnly = fmap (Prelude.filter isChat) deltasDown
  mostRecent <- foldDyn (\a b -> take 30 $ (reverse a) ++ b) [] chatsOnly
  formatted <- mapDyn (fmap (\(Chat _ n m) -> n ++ ": " ++ m)) mostRecent 
  simpleList formatted chatMsg 
  return deltasUp
  where chatMsg v = divClass "chatMessage" $ dynText v

