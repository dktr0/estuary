module Estuary.Page where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.WebDirt.Stream
import Sound.Tidal.Context
import Control.Monad.IO.Class (liftIO)
import Data.Map


widgetToPage :: (MonadWidget t m, Show p, ParamPatternable p) => m (Dynamic t (p,a)) -> m (Dynamic t ParamPattern)
widgetToPage x = do
  y <- el "div" x
  code <- mapDyn fst y
  evalButton <- button "eval"
  display code
  let evalEvent = tagDyn code evalButton
  let paramPattern = fmap toParamPattern evalEvent
  holdDyn (toParamPattern emptySPattern) paramPattern

page :: (MonadWidget t m) => WebDirtStream -> m (Dynamic t ParamPattern) -> m ()
page webDirt x = do
  pattern <- x
  let evalEvent = updated pattern
  performEvent_ $ fmap (liftIO . webDirt) evalEvent

page' :: (MonadWidget t m,Show p, ParamPatternable p) => WebDirtStream -> m (Dynamic t (p,a)) -> m ()
page' webDirt x = do
  pattern <- el "div" x
  mapDyn (fst) pattern >>= display
  evalButton <- button "eval"
  let evalEvent = tagDyn pattern evalButton
  performEvent_ $ fmap (liftIO . webDirt . toParamPattern . fst) evalEvent

multipage :: (MonadWidget t m) => WebDirtStream -> [(String,m (Dynamic t ParamPattern))] -> m ()
multipage webDirt pages = do
  let pageNames = Prelude.map (fst) pages
  let pageList = zipWith (\x y -> (y,x)) pageNames ([0..]::[Int])
  let pageMap = constDyn $ fromList pageList
  dd <- dropdown 0 pageMap def
  let pageChange = fmap (Prelude.map (snd) pages !!) $ _dropdown_change dd
  let showPage = fmap (page webDirt) pageChange
  widgetHold (page webDirt (snd (pages!!0))) showPage
  return ()
