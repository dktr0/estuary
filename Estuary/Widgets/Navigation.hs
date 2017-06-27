{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Navigation where

import Reflex
import Reflex.Dom
import Estuary.Protocol.JSON
import Estuary.WebDirt.Foreign

data Navigation =
  Splash |
  TutorialList |
  Tutorial |
  Solo |
  Lobby |
  Collaborate

-- note: temporary [a] in following is meant to become [Part] where a Part is a ParamPatternable notation

navigation :: MonadWidget t m => Event t EstuaryProtocol ->
  m (Dynamic t [a],Event t EstuaryProtocol,Event t Hint)
navigation wsDown = mdo
  let initialPage = page wsDown Splash
  let rebuild = fmap (page wsDown) navEvents
  w <- widgetHold initialPage rebuild
  let values = joinDyn $ mapDyn (\(x,_,_,_)->x) w
  let wsUp = switchPromptlyDyn $ mapDyn (\(_,x,_,_)->x) w
  let hints = switchPromptlyDyn $ mapDyn (\(_,_,x,_)->x) w
  let navEvents = switchPromptlyDyn $ mapDyn (\(_,_,_,x)->x) w
  return (values,wsUp,hints)

page :: MonadWidget t m => Event t EstuaryProtocol -> Navigation ->
  m (Dynamic t [a],Event t EstuaryProtocol,Event t Hint,Event t Navigation)

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
  text "Lobby placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown Collaborate = do
  text "Collaborate placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)
