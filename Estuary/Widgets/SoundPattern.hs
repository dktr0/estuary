{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.SoundPattern where

import Control.Monad
import Data.Map
import Reflex
import Reflex.Dom
import Estuary.Reflex.Utility
import Estuary.Reflex.Container
import Estuary.Tidal.Types
import Estuary.Widgets.Sound


-- container :: (Ord k, Num k, Show k, Eq v, Show v, MonadWidget t m)
--    => Map k v                                -- a map of initial values
--    -> Event t (Map k (Construction v))       -- construction events (replace/insert/delete)
--    -> Event t (Map k w)                      -- signaling events to be delivered to child widgets
--    -> (v -> Event t w -> m (Dynamic t (v,Event t x)))                -- function to make a widget given initial value and signaling event
--    -> m ( (Dynamic t (Map k v)) , Event t (Map k x) )

multiTextWidget::MonadWidget t m => m (Dynamic t (SoundPattern, Event t ChildSignal))
multiTextWidget = el "div" $ mdo
  let initialMap = empty::Map Int Sound
  addButton <- button' "Add" (1=:Insert (Sound Nothing))
  let constructionEvents = mergeWith union [addButton, deleteEvents]
  (values,events) <- container initialMap constructionEvents (0=:Flash<$ never) textWidget
  values'<- forDyn values (SoundPattern . elems) -- Dyn SoundPattern
  let deleteEvents = fmap (Data.Map.map (\val->Delete)) events
  returnVal <- forDyn values' (\k-> (k,(DeleteMe<$never)))
  display values'
  return returnVal


trivialSoundPattern :: MonadWidget t m => m (Dynamic t (SoundPattern,()))
trivialSoundPattern = el "div" $ do
  x <- button' "bd cp" $ SoundPattern (Prelude.map simpleSound ["bd","cp"])
  y <- button' "arpy*4" $ SoundPattern (Prelude.map simpleSound ["arpy","arpy","arpy","arpy"])
  z <- button' "~ arp" $ SoundPattern [silentSound,simpleSound "arp"]
  pattern <- holdDyn (SoundPattern []) $ leftmost [x,y,z]
  display pattern
  mapDyn (\a -> (a,())) pattern
