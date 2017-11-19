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
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import qualified Estuary.Types.TerminalCommand as Command

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
  let terminalInput = tag (current $ _textInput_value chatInput) send''
  let ? = fmap Command.parseTerminalCommand terminalInput

  -- parse terminal input that does begin with : into other commands to be sent to the server
  let terminalWords = fmap (lowerCaseFirstWord . words) toSend
  let wordsAndViewState = attachDyn viewState terminalWords
  let ensembleRequestsFromTerminal = fmap parseTerminalToRequests wordsAndViewState
  let viewStateChangesFromTerminal = fmap parseTerminalToViewStateChanges wordsAndViewState

  -- parse terminal input that doesn't begin with : into a chat request sent to the server
  let chatEvent = ffilter ((/=':') . (!!0)) toSend
  let chatEvent'' = attachDynWith (\name (site,msg) -> EnsembleRequest (Sited site (SendChat name msg))) (_textInput_value nameInput) chatEvent'

  -- parse responses from server in order to display log/chat messages
  let deltasDown' = fmap justEnsembleResponses deltasDown
  let spaceAndDeltasDown = attachDyn space deltasDown'
  let justInSpace = fmap (\(x,y) -> justSited x $ y) spaceAndDeltasDown
  let messages = fmap (mapMaybe messageForEnsembleResponse) justInSpace
  mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
  simpleList mostRecent $ \v -> divClass "chatMessage" $ dynText v

  let ensembleRequests = leftmost [ensembleRequestsFromTerminal,chatEvent]
  return $ attachDynWith (\site x -> EnsembleRequest (Sited site x)) space ensembleRequests





parseTerminalToRequests :: ([String],ViewState) -> Maybe (EnsembleRequest Definition)
parseTerminalToRequests [":publishview":name:_] vs = Just (PublishView (Sited name (currentView vs)))
parseTerminalToRequests [":listviews":_] _ = Just ListViews
parseTerminalToRequests [":getview":name:_] _ = Just (GetView name)
parseTerminalToRequests _ = Nothing

parseTerminalToViewStateChange :: ([String],ViewState) -> Maybe (ViewState -> ViewState)
parseTerminalToViewStateChange [":publishview":name:_] = Just ... -- name of current view changed, and entry in map updated
parseTerminalToViewStateChange [":newview":_] = Just ... -- name of current view cleared, default view set
parseTerminalToViewStateChange [":addpanel":z:t:_] = withCurrentView addPanel <$> readMaybe z <*> readMaybe t -- this is not right
parseTerminalToViewStateChange [":setpanel":p:z:t:_] = withCurrentView setPanel <$> readMaybe p <*> readMaybe z <*> readMaybe t
parseTerminalToViewStateChange [":deletepanel":p] = withCurrentView deletePanel <$> readMaybe p
parseTerminalToViewStateChange _ _ = Nothing

withCurrentView :: (View -> View) -> ViewState -> ViewState
withCurrentView f vs = vs { currentView = f (currentView vs), currentViewName = ""}

addPanel :: Int -> ViewType -> View -> View
addPanel z t (Views xs) = Views (xs ++ [Panel  ])
addPanel _ _ x = x  -- this guard doesn't make a lot of sense and points to awkward types here

setPanel :: Int -> Int -> ViewType -> View -> View
setPanel p z t (Views xs) = Views ..............

messageForEnsembleResponse :: EnsembleResponse Definition -> Maybe String
messageForEnsembleResponse (Chat name msg) = Just $ name ++ " chats: " ++ msg
messageForEnsembleResponse (ViewList xs) = Just $ "Views: " ++ (show xs)
messageForEnsembleResponse (View (Sited x _)) = Just $ "received view " ++ x
messageForEnsembleResponse _ = Nothing
