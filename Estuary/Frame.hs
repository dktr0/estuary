module Estuary.Frame where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Stream
import Sound.Tidal.Context

-- frame provides a generic container for any of our widgets that produce
-- a ParamPatternable type as part of their output, enabling the output of
-- of the widget to be evaluated as a Tidal pattern and rendered with WebDirt.

frame :: (MonadWidget t m,ParamPatternable p) => WebDirtStream -> m (Dynamic t (p,a)) -> m ()
frame webDirt workspace = do
  evalButton <- button "eval"
  x <- el "div" workspace
  pattern <- mapDyn (fst) x
  let evalEvent = tagDyn (dynamic t ParamPattern) evalButton
  performEvent_ $ fmap (liftIO . stream . sound . p) evalEvent
