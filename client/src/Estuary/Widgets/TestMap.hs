{-# LANGUAGE OverloadedStrings, FlexibleContexts #-} {-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.TestMap where

import Reflex
import Reflex.Dom
import Reflex.Dynamic

import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Data.Time
import Data.Time.LocalTime
import GHCJS.DOM.EventM
import qualified Data.Fixed as F
import Data.Map as Map
import Data.IntMap.Strict
import qualified Data.IntMap as IntMap

import Data.Map.Strict as M
import Data.Maybe as Maybe
import Control.Monad.IO.Class
import Control.Monad(liftM)
import qualified Data.Char as C
import Safe.Foldable (maximumMay)
import qualified Data.List as L

import Estuary.Types.Language
import Estuary.Widgets.W
import Estuary.Types.Definition
import Estuary.Widgets.Reflex
import Estuary.Types.Language
import qualified Estuary.Types.Term as Term

-- data Variable t a = Variable {
--   currentValue :: Dynamic t a,
--   localEdits :: Event t a
--   }
--
-- widgetMapDemo :: IO ()
-- widgetMapDemo = mainWidget $ do
--   let i = Data.IntMap.singleton 0 "text zero"
--   delta <- holdDyn i never
--   x <- el "div" $ testMap delta -- :: Variable t Test
--
--   -- display localEdits issued from widget
--   el "div" $ do
--     text "localEdits: "
--     y <- holdDyn i $ localEdits x -- :: Dynamic t Test
--     dynText $ fmap (T.pack . show) y
--
--   -- display currentValue issued from widget
--   el "div" $ do
--     text "currentValue: "
--     dynText $ fmap (T.pack . show) $ currentValue x -- Dynamic t a

-- when incorporating the deltasDown, the input looses focus
testMapWidget :: MonadWidget t m => Dynamic t Test -> W t m (Variable t Test)
testMapWidget deltasDown = mdo
  let i = IntMap.singleton 0 "text zero"
  delta <- holdDyn i never
  -- i <- sample $ current deltasDown
  -- delta <- holdDyn i (updated deltasDown)
  mapEv <-  widgetMapEventWithAddDelete delta ("newtext" <$ addButton) testRowMaybe
  addButton <- divClass "" $ traceEvent "AddButton" <$> buttonWithClass "+" -- Event t ()
  variable deltasDown mapEv


testRowMaybe :: MonadWidget t m => Dynamic t Text -> W t m (Event t (Maybe Text))
testRowMaybe delta = do
  deleteButton <- button "-"
  row <- el "div" $ (textInputW (constDyn $ "placeholder" =: "") delta) -- Event t Text
  let rowMaybe = fmap Just row -- Event t (Maybe Text)
  return $ leftmost [rowMaybe, Nothing <$ deleteButton]
