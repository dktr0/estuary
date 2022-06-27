{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.AudioMap (audioMapWidget) where

import Reflex
import Reflex.Dom
import Data.Map as Map
import Data.IntMap as IntMap
import Data.Text
import TextShow
import Data.List (nub)
import Data.Tuple.Select
import Control.Monad.Fix

import Estuary.Widgets.W
import Estuary.Resources.LocMap


audioMapWidget :: (Reflex t, Monad m, MonadFix m, Adjustable t m, PostBuild t m, MonadHold t m, DomBuilder t m) => W t m ()
audioMapWidget = elClass "div" "reference" $ do
  aMap <- fmap sel1 <$> resourceMaps -- :: Dynamic t (LocMap Text)
  simpleList (reduceAudioMap <$> aMap) builder
  return ()


reduceAudioMap :: LocMap Text -> [(Text,Int)]
reduceAudioMap = Map.toList . fmap IntMap.size


builder :: (Reflex t, Monad m, DomBuilder t m, PostBuild t m) => Dynamic t (Text,Int)-> m ()
builder x = el "div" $ dynText $ fmap (\(sName,sCount) -> sName <> " (" <> showt sCount <> " samples)") x
