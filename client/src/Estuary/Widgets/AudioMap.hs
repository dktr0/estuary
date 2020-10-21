{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.AudioMap where

import Reflex
import Reflex.Dom
import Data.Map
import Data.Text
import Data.List (nub)

import Estuary.Widgets.W
import Estuary.Types.ResourceMap

audioMapWidget :: MonadWidget t m => W t m ()
audioMapWidget = elClass "div" "reference" $ do
  aMap <- audioMap <$> ensembleC
  simpleList (fmap (nub . (fmap fst) . keys) aMap) builder
  return ()

builder :: MonadWidget t m => Dynamic t Text-> m ()
builder x = el "div" $ dynText x
