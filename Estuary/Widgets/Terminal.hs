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
import Data.Map (fromList)

import Estuary.Protocol.Foreign
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleState
import qualified Estuary.Types.Terminal as Terminal

terminalWidget :: MonadWidget t m =>
  Event t ServerRequest -> Event t [ServerResponse] -> m (Event t Terminal.Command)
terminalWidget deltasUp deltasDown = divClass "terminal" $ mdo
  currentSpace <- mostRecentEnsemble deltasUp
  (sendButton,inputWidget) <- divClass "terminalHeader" $ do
    sendButton' <- divClass "webSocketButtons" $ button "Send"
    divClass "webSocketButtons" $ text "Terminal/Chat:"
    let resetText = fmap (const "") terminalInput
    let attrs = constDyn $ fromList [("class","webSocketTextInputs"),("style","width: 100%")]
    inputWidget' <- divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
    return (sendButton',inputWidget')
  let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
  let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [sendButton,enterPressed]
  let parsedInput = fmap Terminal.parseCommand terminalInput
  let commands = fmapMaybe (either (const Nothing) Just) parsedInput
  let errorMsgs = fmapMaybe (either (Just . (:[]) . ("Error: " ++) . show) (const Nothing)) parsedInput

  -- parse responses from server in order to display log/chat messages
  let deltasDown' = fmap justEnsembleResponses deltasDown
  let spaceAndDeltasDown = attachDyn currentSpace deltasDown'
  let justInSpace = fmap (\(x,y) -> justSited x $ y) spaceAndDeltasDown
  let responseMsgs = fmap (mapMaybe messageForEnsembleResponse) justInSpace
  let messages = mergeWith (++) [responseMsgs,errorMsgs]
  mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
  simpleList mostRecent $ \v -> divClass "chatMessage" $ dynText v

  return commands

mostRecentEnsemble :: (MonadWidget t m, Eq a) => Event t (Request a) -> m (Dynamic t String)
mostRecentEnsemble requests = do
  let ensembleJoins = fmapMaybe f requests
  let ensembleLeaves = fmap (const "") $ ffilter (==LeaveEnsemble) requests
  holdDyn "" $ leftmost [ensembleJoins,ensembleLeaves]
  where
    f (JoinEnsemble x) = Just x
    f _ = Nothing
