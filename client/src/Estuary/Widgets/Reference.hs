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
import Estuary.Types.Request
import Estuary.Widgets.Editor
import Estuary.Reflex.Router
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term

data Reference =
  MainList |
  MiniTidal |
  Punctual |
  CineZer0 |
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
    cinezero <- el "li" $ goTo CineZer0 "CineZer0"
    return $ leftmost [miniTidal, punctual, cinezero]
  el "h3" $ text "View System"
  navEv2 <- el "ul" $ do
    terminalViewCommands <- el "li" $ goTo TerminalViewCommands "Terminal View Commands"
    return $ leftmost [terminalViewCommands]
  let navEv = leftmost [navEv1, navEv2]
  return navEv

referenceWidget MiniTidal = do
  navEv <- goTo MainList "Home"
  el "h3" $ text "MiniTidal Reference"
  miniTidalHelpFile
  return navEv

referenceWidget Punctual = do
  navEv <- goTo MainList "Home"
  el "h3" $ text "Punctual Reference"
  punctualAudioHelpFile
  return navEv

referenceWidget CineZer0 = do
  navEv <- goTo MainList "Home"
  el "h3" $ text "CineZer0 Reference"
  el "p" $ text "Coming soon"
  return navEv

referenceWidget TerminalViewCommands = do
  navEv <- goTo MainList "Home"
  el "h3" $ text "Terminal View Commands"
  terminalViewCommandsHelpFile
  return navEv

goTo :: MonadWidget t m => Reference -> Text -> Editor t m (Event t Reference)
goTo targetPage title = do
  liftM (targetPage <$) $ dynButtonWithChild "reference-link" $ text title
