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
import Estuary.Types.Context
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Hint


terminalWidget :: MonadWidget t m => Dynamic t Context ->
  Event t Request -> Event t [Response] -> Event t Hint -> m (Event t Terminal.Command)
terminalWidget ctx deltasUp deltasDown hints = divClass "terminal" $ mdo
  currentSpace <- mostRecentEnsemble deltasUp
  (sendButton,inputWidget) <- divClass "terminalHeader" $ do
    sendButton' <- divClass "webSocketButtons" $ dynButton =<< translateDyn Term.Send ctx
    divClass "webSocketButtons" $ dynText =<< translateDyn Term.TerminalChat ctx
    let resetText = fmap (const "") terminalInput
    let attrs = constDyn $ fromList [("class","webSocketTextInputs"),("style","width: 100%")]
    inputWidget' <- divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
    return (sendButton',inputWidget')
  let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
  let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [sendButton,enterPressed]
  let parsedInput = fmap Terminal.parseCommand terminalInput
  let commands = fmapMaybe (either (const Nothing) Just) parsedInput
  let errorMsgs = fmapMaybe (either (Just . (:[]) . ("Error: " ++) . show) (const Nothing)) parsedInput

  let hintMsgs = fmap (\x -> [x]) $ fmapMaybe hintsToMessages hints

  -- parse responses from server in order to display log/chat messages
  let deltasDown' = fmap justEnsembleResponses deltasDown
  let spaceAndDeltasDown = attachDyn currentSpace deltasDown'
  let justInSpace = fmap (\(x,y) -> justSited x $ y) spaceAndDeltasDown
  let responseMsgs = fmap (mapMaybe messageForEnsembleResponse) justInSpace
  let messages = mergeWith (++) [responseMsgs,errorMsgs,hintMsgs]
  mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
  simpleList mostRecent $ \v -> divClass "chatMessage" $ dynText v

  return commands

hintsToMessages :: Hint -> Maybe String
hintsToMessages (LogMessage x) = Just x
hintsToMessages _ = Nothing

mostRecentEnsemble :: (MonadWidget t m) => Event t Request -> m (Dynamic t String)
mostRecentEnsemble requests = do
  let ensembleJoins = fmapMaybe f requests
  let ensembleLeaves = fmap (const "") $ ffilter (==LeaveEnsemble) requests
  holdDyn "" $ leftmost [ensembleJoins,ensembleLeaves]
  where
    f (JoinEnsemble x) = Just x
    f _ = Nothing
