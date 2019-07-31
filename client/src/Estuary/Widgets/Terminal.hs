{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Terminal (terminalWidget) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Text.JSON
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Either
import Data.Maybe
import Data.Map (fromList)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Protocol.Foreign
import Estuary.Protocol.Peer
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleC
import Estuary.Types.Context
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Hint


terminalWidget :: MonadWidget t m => Dynamic t Context ->
  Event t [Response] -> Event t [Hint] -> m (Event t Terminal.Command)
terminalWidget ctx deltasDown hints = divClass "terminal" $ mdo
  (sendButton,inputWidget) <- divClass "terminalHeader code-font primary-color" $ do
    sendButton' <- divClass "webSocketButtons" $ dynButton =<< translateDyn Term.Send ctx
    divClass "webSocketButtons" $ dynText =<< translateDyn Term.TerminalChat ctx
    let resetText = fmap (const "") terminalInput
    let attrs = constDyn $ fromList [("class","primary-color code-font"),("style","width: 100%")]
    inputWidget' <- divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
    return (sendButton',inputWidget')
  let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
  let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [sendButton,enterPressed]
  let parsedInput = fmap Terminal.parseCommand terminalInput
  let commands = fmapMaybe (either (const Nothing) Just) parsedInput
  let errorMsgs = fmapMaybe (either (Just . (:[]) . ("Error: " <>) . T.pack . show) (const Nothing)) parsedInput

  let hintMsgs = ffilter (/= []) $ fmap hintsToMessages hints

  -- parse responses from server in order to display log/chat messages
  let deltasDown' = fmap justEnsembleResponses deltasDown
  let responseMsgs = fmap (Data.Maybe.mapMaybe responseToMessage) deltasDown'
  let streamIdMsgs = fmap (\x -> ["new Peer id: " <> x]) streamId
  let messages = mergeWith (++) [responseMsgs,errorMsgs,hintMsgs,streamIdMsgs]
  mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
  simpleList mostRecent $ \v -> divClass "chatMessage code-font primary-color" $ dynText v

  startStreamingReflex ctx $ ffilter (== Terminal.StartStreaming) commands
  streamId <- peerProtocolIdReflex ctx $ ffilter (== Terminal.StreamId) commands

  return commands

hintsToMessages :: [Hint] -> [Text]
hintsToMessages hs = fmapMaybe hintToMessage hs

hintToMessage :: Hint -> Maybe Text
hintToMessage (LogMessage x) = Just x
hintToMessage _ = Nothing

startStreamingReflex :: MonadWidget t m => Dynamic t Context -> Event t a -> m ()
startStreamingReflex ctx e = do
  pp <- fmap peerProtocol $ (sample . current) ctx
  performEvent_ $ fmap (liftIO . const (startStreaming pp)) e

peerProtocolIdReflex :: MonadWidget t m => Dynamic t Context -> Event t a -> m (Event t Text)
peerProtocolIdReflex ctx e = do
  pp <- fmap peerProtocol $ (sample . current) ctx
  performEvent $ fmap (liftIO . const (peerProtocolId pp)) e
