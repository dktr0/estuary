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
import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Hint
import Estuary.Widgets.Editor
import Estuary.Widgets.EnsembleStatus

terminalWidget :: MonadWidget t m => Event t [Response] -> Event t [Hint] -> W t m ()
terminalWidget deltasDown hintsDown = divClass "terminal code-font" $ do
  divClass "chat" $ mdo
    inputWidget <- divClass "terminalHeader code-font primary-color" $ do
      divClass "webSocketButtons" $ term Term.TerminalChat >>= dynText
      let resetText = fmap (const "") terminalInput
      let attrs = constDyn $ fromList [("class","primary-color code-font"),("style","width: 100%")]
      inputWidget' <- divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
      return inputWidget'
    let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
    let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [enterPressed]
    let parsedInput = fmap Terminal.parseCommand terminalInput
    let errorMsgs = fmapMaybe (either (Just . (:[]) . ("Error: " <>) . T.pack . show) (const Nothing)) parsedInput
    let hintMsgs = ffilter (/= []) $ fmap (fmapMaybe hintToMessage) hintsDown
    -- parse responses from server in order to display log/chat messages
    let responseMsgs = fmap (Data.Maybe.mapMaybe responseToMessage) deltasDown
    let streamIdMsgs = fmap (\x -> ["new Peer id: " <> x]) streamId
    let messages = mergeWith (++) [responseMsgs,errorMsgs,hintMsgs,streamIdMsgs]
    mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
    divClass "chatMessageContainer" $ simpleList mostRecent $ \v -> divClass "chatMessage code-font primary-color" $ dynText v
    --
    performCommands $ fmapMaybe (either (const Nothing) Just) parsedInput
  divClass "ensembleStatus" $ ensembleStatusWidget
  return ()


hintToMessage :: Hint -> Maybe Text
hintToMessage (LogMessage x) = Just x
hintToMessage _ = Nothing


-- given Events from the terminal, perform them as IO actions that might signal a Hint, an
-- EnsembleEvent, and/or a Request
performCommands :: MonadWidget t m => Event t Terminal.Command -> W t m ()
performCommands cmdEvent = do
  pp <- liftIO $ newPeerProtocol
  ensC <- ensembleC
  let dynInfo = (\v vs t h -> (v,vs,t,h)) <$> view ensC <*> views ensC <*> tempo ensC <*> userHandle ensC
  x <- performEvent $ fmap liftIO $ attachWithMaybe (performCommand pp) (current dynInfo) cmdEvent
  hint $ fmapMaybe ( ) x
  ensembleEvent $ fmapMaybe ( ) x
  request $ fmapMaybe ( ) x


-- quadruple is current view, map of views, tempo, user handle in ensemble
performCommand :: PeerProtocol -> (Either View Text,Map.Map Text View,Tempo,Text) -> Terminal.Command -> IO (Maybe Hint, Maybe EnsembleEvent, Maybe Request)

performCommand _ _ (Terminal.LocalView x) =
  let eev = ViewEvent (Left x)
  return (Nothing,Just eev,Nothing)

performCommand _ _ (Terminal.PresetView x) = do
  let eev = ViewEvent (Right x)
  return (Nothing,Just eev,Nothing)

performCommand _ (v,vs,_,_) (Terminal.Publishview n) = do
  let eev = ViewsEvent n (activeView v vs)
  return (Nothing,Just eev,Nothing)

performCommand _ (v,_,_,_) Terminal.ActiveView = do
  let h = LogMessage $ nameOfActiveView v
  return (Just h,Nothing,Nothing)

performCommand _ (_,vs,_,_) Terminal.ListViews = do
  let h = LogMessage $ listViews vs
  return (Just h,Nothing,Nothing)

performCommand _ (v,vs,_,_) Terminal.DumpView = do
  let h = LogMessage $ dumpView $ activeView v vs
  return (Just h,Nothing,Nothing)

performCommand _ (_,_,_,h) (Terminal.Chat x) = do
  t <- getCurrentTime
  let eev = ChatEvent (Chat t h x)
  return (Nothing,Just eev,Nothing)

performCommand pp _ Terminal.StartStreaming = do
  startStreaming pp
  return (Nothing,Nothing,Nothing)

performCommand pp _ Terminal.StreamId = do
  x <- peerProtocolId pp
  let h = LogMessage x
  return (Just h,Nothing,Nothing)

performCommand pp _ (Terminal.Delay x) = do
  let h = SetGlobalDelayTime x
  return (Just h,Nothing,Nothing)

performCommand _ _ (Terminal.DeleteThisEnsemble pwd) = do
  let eev = DeleteThisEnsemble pwd
  return (Nothing,Just eev,Nothing)

performCommand _ _ (Terminal.DeleteEnsemble ensName pwd) = do
  let rq = DeleteEnsemble ensName pwd
  return (Nothing,Nothing,Just rq)

performCommand _ _ Terminal.AncientTempo = do
  let eev = TempoEvent $ Tempo { freq = 0.5, time = UTCTime (fromGregorian 2020 01 01) 0, count = 0 }
  return (Nothing,Just eev,Nothing)

performCommand _ (_,_,t,_) Terminal.ShowTempo = do
  let h = LogMessage $ T.pack $ show t
  return (Just h,Nothing,Nothing)

performCommand _ (_,_,t,_) (Terminal.SetCPS x) = do
  x' <- changeTempoNow (realToFrac x) t
  let eev = TempoEvent x'
  return (Nothing,Just eev,Nothing)

performCommand _ (_,_,t,_) (Terminal.SetBPM x) = do
  x' <- changeTempoNow (realToFrac x / 240) t
  let eev = TempoEvent x'
  return (Nothing,Just eev,Nothing)

performCommand _ _ (Terminal.InsertAudioResource url bankName n) = do
  let eev = InsertAudioResource url bankName n
  return (Nothing,Just eev,Nothing)

performCommand _ _ (Terminal.DeleteAudioResource bankName n) = do
  let eev = DeleteAudioResource bankName n
  return (Nothing,Just eev,Nothing)

performCommand _ _ (Terminal.AppendAudioResource url bankName) = do
  let eev = AppendAudioResource url bankName
  return (Nothing,Just eev,Nothing)
