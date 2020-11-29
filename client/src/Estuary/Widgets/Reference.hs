{-# LANGUAGE OverloadedStrings, RecursiveDo, DeriveGeneric, DeriveAnyClass #-}

module Estuary.Widgets.Reference (Reference(..),navigation) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Control.Monad
import GHC.Generics
import GHCJS.Marshal
import Data.Text (Text)

import Estuary.Widgets.Generic
import Estuary.Help.LanguageHelp
import Estuary.Help.MiniTidal
import Estuary.Help.PunctualAudio
import Estuary.Help.TerminalViewCommands
import Estuary.Help.Hydra
import Estuary.Help.CineCer0.CineCer0
import Estuary.Help.CineCer0.CineCer0Reference
import Estuary.Types.Request
import Estuary.Widgets.Editor
import Estuary.Reflex.Router
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term

data Reference =
  MainList |
  MiniTidal |
  Punctual |
  Hydra |
  CineCer0 |
  CineCer0Reference |
  TerminalViewCommands
  deriving (Generic, FromJSVal, ToJSVal)

navigation :: MonadWidget t m => Editor t m (Event t Reference)
navigation = do
  x <- router' MainList never $ referenceWidget
  let a = switchDyn x
  return a

referenceWidget :: MonadWidget t m => Reference -> Editor t m (Event t Reference)

referenceWidget MainList = do
  divClass "reference-title" $ text "estuary"
  divClass "reference-description" $ text "Estuary is a platform for collaboration and learning through live coding. It enables you to create sound, music, and visuals in a web browser."
  el "h3" $ text "Languages"
  navEv1 <- el "ul" $ do
    miniTidal <- el "li" $ goTo MiniTidal "MiniTidal"
    punctual <- el "li" $ goTo Punctual "Punctual"
    hydra <- el "li" $ goTo Hydra "Hydra"
    cineCero <- el "li" $ goTo CineCer0 "CineCer0"
    return $ leftmost [miniTidal, punctual, hydra, cineCero]
  el "h3" $ text "View System"
  navEv2 <- el "ul" $ do
    terminalViewCommands <- el "li" $ goTo TerminalViewCommands "Terminal View Commands"
    return $ leftmost [terminalViewCommands]
  let navEv = leftmost [navEv1, navEv2]
  return navEv

referenceWidget MiniTidal = do
  navEv <- goTo MainList "Home"
  el "h3" $ text "MiniTidal"
  miniTidalHelpFile
  return navEv

referenceWidget Punctual = do
  navEv <- goTo MainList "Home"
  el "h3" $ text "Punctual"
  punctualAudioHelpFile
  return navEv

referenceWidget Hydra = do
  navEv <- goTo MainList "Home"
  el "h3" $ text "Hydra"
  hydraHelp
  return navEv

referenceWidget CineCer0 = do
  navEv <- el "div" $ do
    home <- goTo MainList "Home"
    cinecer0Reference <- goTo CineCer0Reference "CineCer0 Reference"
    return $ leftmost [home, cinecer0Reference]
  el "h3" $ text "CineCer0"
  cineCer0Help
  return navEv

referenceWidget CineCer0Reference = do
  navEv <- el "div" $ do
    home <- goTo MainList "Home"
    cinecer0 <- goTo CineCer0 "CineCer0"
    return $ leftmost [home, cinecer0]
  el "h3" $ text "CineCer0 Reference"
  cineCer0Reference
  return navEv

referenceWidget TerminalViewCommands = do
  navEv <- goTo MainList "Home"
  el "h3" $ text "Terminal View Commands"
  terminalViewCommandsHelpFile
  return navEv

goTo :: MonadWidget t m => Reference -> Text -> Editor t m (Event t Reference)
goTo targetPage title = do
  liftM (targetPage <$) $ dynButtonWithChild "reference-link" $ text title
