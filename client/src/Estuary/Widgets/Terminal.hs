{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Terminal (terminalWidget) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Maybe
import Data.Map.Strict (fromList)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Protocol.Peer
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleC
import Estuary.Types.Context
import Estuary.Reflex.Utility
import Estuary.Render.DynamicsMode
import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Hint
import Estuary.Widgets.Editor
import Estuary.Widgets.EnsembleStatus

terminalWidget :: MonadWidget t m => Event t [Response] -> Event t [Hint] -> Editor t m (Event t Terminal.Command)
terminalWidget deltasDown hints = divClass "terminal code-font" $ mdo
  commands <- divClass "chat" $ mdo
    (inputWidget) <- divClass "terminalHeader code-font primary-color" $ do
      divClass "webSocketButtons" $ term Term.TerminalChat >>= dynText
      let resetText = fmap (const "") terminalInput
      let attrs = constDyn $ fromList [("class","primary-color code-font"),("style","width: 100%")]
      inputWidget' <- divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
      return (inputWidget')
    let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
    let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [enterPressed]
    let parsedInput = fmap Terminal.parseCommand terminalInput
    let commands = fmapMaybe (either (const Nothing) Just) parsedInput
    let errorMsgs = fmapMaybe (either (Just . (:[]) . ("Error: " <>) . T.pack . show) (const Nothing)) parsedInput

    let hintMsgs = ffilter (/= []) $ fmap hintsToMessages hints

    -- parse responses from server in order to display log/chat messages
    let responseMsgs = fmap (Data.Maybe.mapMaybe responseToMessage) deltasDown
    let streamIdMsgs = fmap (\x -> ["new Peer id: " <> x]) streamId
    let messages = mergeWith (++) [responseMsgs,errorMsgs,hintMsgs,streamIdMsgs]
    mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
    divClass "chatMessageContainer" $ simpleList mostRecent $ \v -> divClass "chatMessage code-font primary-color" $ dynText v

    pp <- liftIO $ newPeerProtocol
    mb <- mainBus <$> immutableRenderContext
    performCommands pp mb commands
    streamId <- peerProtocolIdReflex pp $ ffilter (== Terminal.StreamId) commands
    return commands

  divClass "ensembleStatus" $ ensembleStatusWidget

  return commands

hintsToMessages :: [Hint] -> [Text]
hintsToMessages hs = fmapMaybe hintToMessage hs

hintToMessage :: Hint -> Maybe Text
hintToMessage (LogMessage x) = Just x
hintToMessage _ = Nothing

performCommands :: MonadWidget t m => PeerProtocol -> MainBus -> Event t Terminal.Command -> m ()
performCommands pp mb x = performEvent_ $ fmap (liftIO . doCommands pp mb) x

doCommands :: PeerProtocol -> MainBus -> Terminal.Command -> IO ()
doCommands _ mb (Terminal.MonitorInput x) = changeMonitorInput mb x
doCommands pp _ Terminal.StartStreaming = startStreaming pp
doCommands _ _ _ = return ()

peerProtocolIdReflex :: MonadWidget t m => PeerProtocol -> Event t a -> m (Event t Text)
peerProtocolIdReflex pp e = performEvent $ fmap (liftIO . const (peerProtocolId pp)) e
