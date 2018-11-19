{-# LANGUAGE RecursiveDo #-}
module Estuary.Test.Estuary where

import Reflex.Dom
import Data.Time

import Control.Concurrent.MVar
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt

import Estuary.Protocol.Foreign

import Estuary.Widgets.Estuary
import Estuary.Widgets.Terminal
import Estuary.Widgets.Navigation
import Estuary.Widgets.WebSocket

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Context
import Estuary.Types.Hint
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Terminal
import Estuary.Types.Tempo

import Estuary.WebDirt.SampleEngine
import Estuary.RenderInfo
import Estuary.RenderState
import Estuary.Renderer

import Estuary.Test.Reflex

-- estuaryWidget -> deltasUp is an event for web socket events
--  they are passed to the EstuaryProtocolObject
--  alternateProtocol in WebSocket serializes the Requests

controllableNavigationWidget :: MonadWidget t m => Navigation -> Dynamic t Context -> Dynamic t RenderInfo -> Event t Command -> Event t [Response] -> m (Dynamic t DefinitionMap, Event t Request, Event t Hint, Event t Tempo)
controllableNavigationWidget pageId ctx renderInfo commands wsDown = mdo
  let initialPage = page ctx renderInfo commands wsDown pageId
  let rebuild = fmap (page ctx renderInfo commands wsDown) navEvents
  w <- widgetHold initialPage rebuild
  values <- liftM joinDyn $ mapDyn (\(x,_,_,_,_)->x) w
  wsUp <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_,_,_)->x) w
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x,_,_)->x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,_,x,_)->x) w
  tempoEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,_,_,x)->x) w
  return (values, wsUp, hints, tempoEvents)

controllableEstuaryWidget :: MonadWidget t m => Navigation -> MVar Context -> MVar RenderInfo -> EstuaryProtocolObject -> m ()
controllableEstuaryWidget pageId ctxVar renderInfoVar protocol = divClass "estuary" $ mdo
  renderInfo <- pollRenderInfoChanges renderInfoVar

  headerChanges <- header ctx renderInfo

  (values, deltasUp, hints, tempoChanges) <- divClass "page" $
    controllableNavigationWidget pageId ctx renderInfo commands deltasDown'

  commands <- divClass "chat" $
    terminalWidget ctx deltasUp deltasDown'

  (deltasDown, wsStatus) <- alternateWebSocket protocol deltasUp

  let definitionChanges = fmap setDefinitions $ updated values
  let deltasDown' = ffilter (not . Prelude.null) deltasDown
  let ccChange = fmap setClientCount $ fmapMaybe justServerClientCount deltasDown'
  let tempoChanges' = fmap (\t x -> x { tempo = t }) tempoChanges
  let contextChanges = mergeWith (.) [definitionChanges, headerChanges, ccChange, tempoChanges']

  initialCtx <- liftIO $ readMVar ctxVar
  ctx <- foldDyn ($) initialCtx contextChanges -- Dynamic t Context

  themeChangeEv <- fmap updated $ mapDyn theme ctx -- Event t String

  changeTheme themeChangeEv
  updateContext ctxVar ctx
  performHint (webDirt initialCtx) hints

silentEstuaryWithInitialPage :: EstuaryProtocolObject -> Navigation -> IO ()
silentEstuaryWithInitialPage protocol pageId = do
  initialCtx <- initCtx
  ctxVar <- newMVar $ initialCtx

  renderInfoVar <- newMVar $ emptyRenderInfo
  forkRenderThread ctxVar renderInfoVar

  renderSync_ $
    controllableEstuaryWidget pageId ctxVar renderInfoVar protocol

silentEstuary :: EstuaryProtocolObject -> IO ()
silentEstuary protocol = do
  ic <- initCtx
  c <- newMVar $ ic
  ri <- newMVar $ emptyRenderInfo
  forkRenderThread c ri

  renderSync_ $ estuaryWidget c ri protocol

initCtx :: IO Context
initCtx = do
  now <- Data.Time.getCurrentTime
  let ctx = initialContext now js_nullWebDirt js_nullSuperDirt
  return $ ctx {
    webDirtOn = False
  }

foreign import javascript safe
  "null"
  js_nullWebDirt :: WebDirt

foreign import javascript safe
  "null"
  js_nullSuperDirt :: SuperDirt
