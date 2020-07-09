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

navigation :: MonadWidget t m => Editor t m (Event t Request)
navigation = do
  x <- router' MainList never $ referenceWidget
  let y = fmap snd x
  let a = switchDyn $ fmap fst y
  return a

referenceWidget :: MonadWidget t m => Reference -> Editor t m (Event t Reference)

referenceWidget MainList = do
  navEv <- divClass "reference" $ do
    divClass "reference-title" $ text "estuary"
    divClass "reference-description" $ text "Estuary is a platform for collaboration and learning through live coding. It enables you to create sound, music, and visuals in a web browser."
    el "h3" $ text "Languages"
    miniTidal <- goTo "reference-margin" MiniTidal "MiniTidal"
    punctual <- goTo "reference-margin" Punctual "Punctual"
    cinezero <- goTo "reference-margin" CineZer0 "CineZer0"
    return $ leftmost [miniTidal, punctual, cinezero]
  return navEv

referenceWidget MiniTidal = do
  text "MiniTidal Help"
  return never

referenceWidget Punctual = do
  text "Punctual Help"
  return never

referenceWidget MiniTidal = do
  text "CineZer0 Help"
  return never

goTo :: MonadWidget t m => Text -> Reference -> Text -> Editor t m (Event t Reference)
goTo c targetPage title = divClass c $ do
  liftM (targetPage <$) $ dynButtonWithChild "reference-link" $ do
    divClass "reference-title-container" $ divClass "reference-title" $ text title
