{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Sequencer where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Control.Monad
import Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Safe.Foldable (maximumMay)
import Text.Read (readMaybe)
import Estuary.Types.Hint
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility


type Sequence a = Map Int (Maybe a,[Bool])

attachIndex :: [a] -> [(Int,a)]
attachIndex = zip [0..]


sequencer :: MonadWidget t m => Map Int (String, [Bool]) -> Event t (Map Int (String,[Bool])) -> m (Dynamic t (Map Int (String,[Bool])), Event t (Map Int (String,[Bool])), Event t Hint)
sequencer iMap update = elClass "table" "sequencer" $ mdo
  let seqLen = maximum $ fmap (length . snd) $ elems iMap
  let serverDeletes = fmap (Nothing <$) $ attachWith M.difference (current values) update -- for deleted rows
  let updateEvs = mergeWith union [fmap (fmap Just) update, serverDeletes]
  let downstreamEvs = leftmost [updateEvs, newRow, deleteEvents] -- Event t (Map Int (Maybe (String,[Bool])))
  ourList <- listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow)
    -- Dynamic t (Map Int (Dynamic t ((String,[Bool]),Event t ())))
  let widgets = joinDynThroughMap ourList -- Dyn t (Map Int ((String,[Bool]), Event t ()))
    -- Dynamic t (Map Int ((String,[Bool]),Event t ()))
  let values = fmap (fmap fst) widgets
    -- Dynamic t (Map Int ((String,[Bool])))
  let dynEvs = fmap (mergeMap . fmap (snd)) widgets -- Event t (Map Int ())
  let deleteEvents = fmap (Nothing <$) $ switch $ current $ dynEvs
  newValues <- holdUniqDyn values
  let updateVal = fmapMaybe id $ mergeWith (\a b->Nothing) [Nothing <$ update, fmap Just $ updated newValues]
  let maxKey = fmap (maybe 0 id . maximumMay . keys) newValues
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> singleton (k+1) (Just ("",Prelude.take seqLen $ repeat False))) (current maxKey) plusButton
  return (values,updateVal,never)

-- listWithKeyShallowDiff
--  :: (Ord k, Adjustable t m, MonadFix m, MonadHold t m)
--  => Map k v
--  -> Event t (Map k (Maybe v))
--  -> (k -> v -> Event t v -> m a)
--  -> m (Dynamic t (Map k a))
-- joinDynThroughMap :: forall t k a. (Reflex t, Ord k) => Dynamic t (Map k (Dynamic t a)) -> Dynamic t (Map k a)

-- Event returned is a message to delete that row
sequencerRow ::(MonadWidget t m) => (String,[Bool]) -> Event t (String,[Bool]) -> m (Dynamic t ((String,[Bool]), Event t ()))
sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
  let buttonIVals = M.fromList $ attachIndex vals
  let strUpdate = fmap (T.pack . fst) edits
  let buttonUpdates = fmap (M.fromList . attachIndex . fmap Just . snd) edits
  let textInputAttrs = singleton "class" "sequencer-textarea code-font other-borders"
  deleteMe <- elClass "td" "delete" $ dynButton "-" -- clickableTdClass (constDyn " - ") (constDyn "delete") ()
  rowInput <- elClass "td"  "sequencer-textarea code-font primary-color other-borders" $ textInput $ def & textInputConfig_initialValue .~ (T.pack iVal) & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn ("class" =: "sequencer-textarea  code-font primary-color"))
  -- rowInput <- el "td" $ growingTextInput $ def & textInputConfig_initialValue .~ iVal & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn textInputAttrs)
  buttons <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals buttonUpdates sequencerButton  -- Dyn (Map Int Bool)
  let val = (\s b -> (T.unpack s, elems b)) <$> (_textInput_value rowInput) <*> buttons
  return $ fmap (\x->(x,deleteMe)) val

sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t Bool)
sequencerButton pos val edits = mdo
  (element,_) <- elDynAttr' "td" attrs $ return ()
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Mousedown) (return ())
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]
  let attrs = fmap (\x-> singleton "class" $ if x then "sequencerButton sequencerButton-activated primary-color other-borders" else "sequencerButton other-borders") isActive
  return isActive
