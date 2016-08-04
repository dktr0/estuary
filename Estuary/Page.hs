module Estuary.Page where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.WebDirt.Stream
import Sound.Tidal.Context
import Control.Monad.IO.Class (liftIO)
import Data.Map


widgetToPage :: (MonadWidget t m, ParamPatternable p) => m (Dynamic t (p,a)) -> m (Dynamic t ParamPattern)
widgetToPage x = x >>= mapDyn (toParamPattern . fst)


page :: (MonadWidget t m) => WebDirtStream -> m (Dynamic t ParamPattern) -> m ()
page webDirt x = do
  pattern <- x
  evalButton <- button "eval"
  let evalEvent = tagDyn pattern evalButton
  performEvent_ $ fmap (liftIO . webDirt) evalEvent


page' :: (MonadWidget t m,ParamPatternable p) => WebDirtStream -> m (Dynamic t (p,a)) -> m ()
page' webDirt x = page webDirt (widgetToPage x)


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
