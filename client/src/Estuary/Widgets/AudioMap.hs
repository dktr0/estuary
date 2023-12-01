{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.AudioMap (audioMapWidget) where

import Reflex
import Reflex.Dom
import Data.Map as Map
import Data.IntMap as IntMap
import Data.Text
import TextShow
import Data.List (nub)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class

import Estuary.Widgets.W
import Estuary.Resources.LocMap
import Estuary.Widgets.Reflex (traceDynamic)
import Estuary.Resources (audioMap)

audioMapWidget :: (Reflex t, Monad m, MonadFix m, Adjustable t m, PostBuild t m, MonadHold t m, DomBuilder t m) => W t m ()
audioMapWidget = elClass "div" "reference" $ do
  aMap <- fmap audioMap <$> resourceMaps -- :: Dynamic t (LocMap Text)
  simpleList (reduceAudioMap <$> aMap) builder
  return ()


reduceAudioMap :: LocMap Text -> [(Text,Int)]
reduceAudioMap = Map.toList . fmap IntMap.size


builder :: (Reflex t, Monad m, DomBuilder t m, PostBuild t m) => Dynamic t (Text,Int)-> m ()
builder x = el "div" $ dynText $ fmap (\(sName,sCount) -> sName <> " (" <> showt sCount <> " samples)") x
