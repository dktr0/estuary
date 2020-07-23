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
import Estuary.Types.Request
import Estuary.Widgets.Editor
import Estuary.Reflex.Router
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term

data Reference =
  MainList |
  MiniTidal |
  Punctual |
  CineZer0
  deriving (Generic, FromJSVal, ToJSVal)

navigation :: MonadWidget t m => Editor t m (Event t Reference)
navigation = do
  x <- router' MainList never $ referenceWidget
  let a = switchDyn x
  return a

referenceWidget :: MonadWidget t m => Reference -> Editor t m (Event t Reference)

referenceWidget MainList = do
  navEv <- divClass "reference" $ do
    divClass "reference-title" $ text "estuary"
    divClass "reference-description" $ text "Estuary is a platform for collaboration and learning through live coding. It enables you to create sound, music, and visuals in a web browser."
    el "h3" $ text "Languages"
    navEv' <- el "ul" $ do
      miniTidal <- el "li" $ goTo "reference-margin" MiniTidal "MiniTidal"
      punctual <- el "li" $ goTo "reference-margin" Punctual "Punctual"
      cinezero <- el "li" $ goTo "reference-margin" CineZer0 "CineZer0"
      return $ leftmost [miniTidal, punctual, cinezero]
    return navEv'
  return navEv

referenceWidget MiniTidal = do
  navEv <- goTo "reference-margin" MainList "⇦"
  divClass "reference-page" $ el "h3" $ text "MiniTidal Help"
  el "p" $ text "Placeholder text"
  return navEv

referenceWidget Punctual = do
  navEv <- goTo "reference-margin" MainList "⇦"
  divClass "reference-page" $ el "h3" $ text "Punctual Help"
  el "p" $ text "Placeholder text"
  return navEv

referenceWidget CineZer0 = do
  navEv <- goTo "reference-margin" MainList "⇦"
  divClass "reference-page" $ el "h3" $ text "CineZer0 Help"
  el "p" $ text "Placeholder text"
  return navEv

goTo :: MonadWidget t m => Text -> Reference -> Text -> Editor t m (Event t Reference)
goTo c targetPage title = divClass c $ do
  liftM (targetPage <$) $ dynButtonWithChild "reference-link" $ text title
