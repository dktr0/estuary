{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Terminal (terminalWidget) where

import Reflex
import Reflex.Dom
import Text.JSON
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Either
import Data.Maybe

import Estuary.Protocol.Foreign
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Terminal

terminalWidget :: MonadWidget t m => Dynamic t String -> Event t [ServerResponse] -> m (Event t Command)
terminalWidget space deltasDown = mdo
  let attrs = constDyn ("class" =: "webSocketTextInputs")
  text "Name:"
  nameInput <- textInput $ def & textInputConfig_attributes .~ attrs
  text "Terminal/Chat:"
  let resetText = fmap (const "") terminalInput
  inputWidget <- textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
  sendButton <- divClass "webSocketButtons" $ button "Send"
  let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
  let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [sendButton,enterPressed]
  let parsedInput = fmap parseCommand terminalInput
  let commands = fmapMaybe (either (const Nothing) Just) parsedInput
  let errorMsgs = fmapMaybe (either (Just . (:[]) . ("Error: " ++) . show) (const Nothing)) parsedInput

  -- parse responses from server in order to display log/chat messages
  let deltasDown' = fmap justEnsembleResponses deltasDown
  let spaceAndDeltasDown = attachDyn space deltasDown'
  let justInSpace = fmap (\(x,y) -> justSited x $ y) spaceAndDeltasDown
  let responseMsgs = fmap (mapMaybe messageForEnsembleResponse) justInSpace
  let messages = mergeWith (++) [responseMsgs,errorMsgs]
  mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
  simpleList mostRecent $ \v -> divClass "chatMessage" $ dynText v

  return commands

messageForEnsembleResponse :: EnsembleResponse Definition -> Maybe String
messageForEnsembleResponse (Chat name msg) = Just $ name ++ " chats: " ++ msg
messageForEnsembleResponse (ViewList xs) = Just $ "Views: " ++ (show xs)
messageForEnsembleResponse (View (Sited x _)) = Just $ "received view " ++ x
messageForEnsembleResponse _ = Nothing
