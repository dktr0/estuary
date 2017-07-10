{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Navigation where

import Reflex
import Reflex.Dom
import Estuary.Protocol.JSON
import Estuary.WebDirt.Foreign
import Estuary.Tidal.Types
import Estuary.Widgets.Generic
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Control.Monad (liftM)
import Data.Map
import Text.Read
import Text.JSON


data Navigation =
  Splash |
  TutorialList |
  Tutorial |
  Solo |
  Lobby |
  Collaborate String


navigation :: MonadWidget t m => Event t [ServerResponse] ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint)
navigation wsDown = mdo
  let initialPage = page wsDown Splash
  let rebuild = fmap (page wsDown) navEvents
  w <- widgetHold initialPage rebuild
  values <- liftM joinDyn $ mapDyn (\(x,_,_,_)->x) w
  wsUp <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_,_)->x) w
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x,_)->x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,_,x)->x) w
  return (values,wsUp,hints)

page :: MonadWidget t m => Event t [ServerResponse] -> Navigation ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint,Event t Navigation)

page wsDown Splash = do
  x <- liftM (TutorialList <$)  $ button "Tutorials"
  y <- liftM (Solo <$)  $ button "Solo"
  z <- liftM (Lobby <$)  $ button "Collaborate"
  let navEvents = leftmost [x,y,z]
  return (constDyn [],never,never,navEvents)

page wsDown TutorialList = do
  text "TutorialList placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown Tutorial = do
  text "Tutorial placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown Solo = do
  text "Solo placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown Lobby = do
  requestSpaceList <- liftM (RequestSpaceList <$) getPostBuild
  spaceList <- holdDyn [] $ justSpaceList <$> wsDown
  join <- simpleList spaceList joinButton -- m (Dynamic t [Event t Navigation])
  join' <- mapDyn leftmost join -- m (Dynamic t (Event t Navigation))
  let join'' = switchPromptlyDyn join' -- Event t Navigation
  back <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],requestSpaceList,never,leftmost [back,join''])

page wsDown (Collaborate w) = do
  (patternMap,wsUp,hints) <- viewInSpaceWidget spaceName defaultView wsDown
  patterns <- mapDyn elems patternMap
  x <- liftM (Lobby <$) $ button "back to lobby"
  return (patterns,wsUp,hints,x)

joinButton :: MonadWidget t m => Dynamic t String -> m (Event t Navigation)
joinButton x = do
  b <- clickableDivClass'' x "placeholderClass" ()
  return $ Collaborate <$> tagDyn x b

{-
tempoWidget :: MonadWidget t m => Event t [ServerResponse] -> m (Event t ServerRequest)
tempoWidget deltas = do
  text "CPS:"
  let delta' = fmap (Prelude.filter isCps) deltas
  let delta'' = fmapMaybe lastOrNothing delta'
  let delta''' = fmapMaybe getCps delta''
  let delta'''' = fmap show delta'''
  t <- textInput $ def & textInputConfig_setValue .~ delta''''
  let t' = fmapMaybe (readMaybe) $ _textInput_input t
  let edits = fmap (TempoChange "") t'
  return edits
-}

diagnostics :: MonadWidget t m =>
  Dynamic t (Map Int TransformedPattern) ->
  Event t ServerRequest ->
  Event t [ServerResponse] ->
  Event t Hint ->
  m ()
diagnostics values deltasUp deltasDown hints = do
  el "div" $ do
    text "Values:"
    mapDyn encode values >>= display
  el "div" $ do
    text "DeltasUp:"
    (holdDyn "" $ fmap encode deltasUp) >>= display
  el "div" $ do
    text "DeltasDown:"
    (holdDyn "" $ fmap encode deltasDown) >>= display
  el "div" $ do
    text "Hints:"
    (holdDyn "" $ fmap show hints) >>= display
