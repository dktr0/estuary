{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.CineCer0 where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Reflex.Utility
import Estuary.Widgets.Editor
import Data.Map.Strict

cineCer0Help :: MonadWidget t m => Editor t m ()
cineCer0Help = el "div" $ do
  aboutCineCer0


aboutCineCer0 :: MonadWidget t m => Editor t m ()
aboutCineCer0 = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"Coming soon"),
    (Español,"Pronto más información")
    ])
