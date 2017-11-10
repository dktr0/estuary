{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Chat (chatWidget) where

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

chatWidget :: MonadWidget t m => Dynamic t String -> Event t [ServerResponse] -> m (Event t ServerRequest)
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
  let toSend' = attachDyn space toSend
  let deltasUp = attachDynWith (\name (site,msg) -> EnsembleRequest (Sited site (SendChat name msg))) (_textInput_value nameInput) toSend'
  -- filter for chat messages in current space only
  let deltasDown' = fmap justEnsembleResponses deltasDown
  let spaceAndDeltasDown = attachDyn space deltasDown'
  let justInSpace = fmap (\(x,y) -> justSited x $ y) spaceAndDeltasDown
  let chatsOnly = fmap justChats justInSpace
  -- display
  mostRecent <- foldDyn (\a b -> take 8 $ (reverse a) ++ b) [] chatsOnly
  formatted <- mapDyn (fmap (\(n,m) -> n ++ ": " ++ m)) mostRecent
  simpleList formatted chatMsg
  return deltasUp
  where chatMsg v = divClass "chatMessage" $ dynText v
