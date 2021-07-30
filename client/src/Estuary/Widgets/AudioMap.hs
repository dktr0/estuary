{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.AudioMap (audioMapWidget) where

import Reflex
import Reflex.Dom
import Data.Map
import Data.Text
import TextShow
import Data.List (nub)

import Estuary.Widgets.W
import Estuary.Types.Context
import Estuary.Resources.AudioResource

audioMapWidget :: MonadWidget t m => W t m ()
audioMapWidget = elClass "div" "reference" $ do
  ctx <- context
  -- aMap <- holdUniqDyn $ fmap audioMap ctx
    -- ???? audiomap is not in context anymore but in ImmutableRenderContext (resources field)
    -- so how do we handle this ???
  -- simpleList (reduceAudioMap <$> aMap) builder
  return ()

reduceAudioMap :: Map (Text,Int) AudioResource -> [(Text,Int)]
reduceAudioMap x =
  let allNames = fmap fst $ keys x -- with duplicates
      sNames = nub allNames -- without duplicates
      sCounts = fmap (countName allNames) sNames
  in Prelude.zip sNames sCounts

countName :: [Text] -> Text -> Int
countName haystack needle = Prelude.length $ Prelude.filter (==needle) haystack

builder :: MonadWidget t m => Dynamic t (Text,Int)-> m ()
builder x = el "div" $ dynText $ fmap (\(sName,sCount) -> sName <> " (" <> showt sCount <> " samples)") x
