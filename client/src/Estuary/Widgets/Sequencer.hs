{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.Sequencer where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Control.Monad
import Data.Map as M
import Safe.Foldable (maximumMay)
import Text.Read (readMaybe)
import Estuary.Types.Hint
import Estuary.Widgets.Generic


type Sequence a = Map Int (Maybe a,[Bool])

attachIndex:: [a] -> [(Int,a)]
attachIndex l = zip (take (length l) [0..]) l


sequencer ::MonadWidget t m => Map Int (String, [Bool]) -> Event t (Map Int (String,[Bool])) -> m (Dynamic t (Map Int (String,[Bool]), Event t (Map Int (String,[Bool])), Event t Hint))
sequencer iMap update = elClass "table" "sequencer" $ mdo
  let seqLen = maximum $ fmap (length . snd) $ elems iMap
  let serverDeletes = fmap (Nothing <$) $ attachWith M.difference (current values) update -- for deleted rows
  let updateEvs = mergeWith union [fmap (fmap Just) update, serverDeletes]
  let downstreamEvs = leftmost [updateEvs, newRow, deleteEvents] -- Event t (Map Int (Maybe (String,[Bool])))
  widgets <- liftM joinDynThroughMap $ listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow) -- Dyn t (Map Int ((String,[Bool]), Event t ()))
  values <- mapDyn (fmap fst) widgets -- Dyn t (Map Int (String,[Bool]))
  dynEvs <- mapDyn (mergeMap . fmap (snd)) widgets -- Event t (Map Int ())
  let deleteEvents = fmap (Nothing <$) $ switch $ current $ dynEvs
  let updateVal = fmapMaybe id $ mergeWith (\a b->Nothing) [Nothing <$ update, fmap Just $ updated (nubDyn values)]
  maxKey <- mapDyn (maybe 0 id . maximumMay . keys) $ nubDyn values
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> singleton (k+1) (Just ("",take seqLen $ repeat False))) (current maxKey) plusButton
  mapDyn (\v-> (v, updateVal, never)) values


-- Event returned is a message to delete that row
sequencerRow ::(MonadWidget t m) => (String,[Bool]) -> Event t (String,[Bool]) -> m (Dynamic t ((String,[Bool]), Event t ()))
sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
  let buttonIVals = M.fromList $ attachIndex vals
  let strUpdate = fmap fst edits
  let buttonUpdates = fmap (M.fromList . attachIndex . fmap Just . snd) edits
  let textInputAttrs = singleton "class" "sequencerTextInputTd"
  deleteMe <- clickableTdClass (constDyn " - ") (constDyn "delete") ()
  rowInput <- elClass "td" "sequencerTextInputTd" $ textInput $ def & textInputConfig_initialValue .~ iVal & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn empty)
  -- rowInput <- el "td" $ growingTextInput $ def & textInputConfig_initialValue .~ iVal & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn textInputAttrs)
  buttons <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals buttonUpdates sequencerButton  -- Dyn (Map Int Bool)
  val <- combineDyn (\s b -> (s, elems b)) (_textInput_value rowInput) buttons
  mapDyn (\x->(x,deleteMe)) val

sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t Bool)
sequencerButton pos val edits = mdo
  (element,_) <- elDynAttr' "td" attrs $ return ()
  clickEv <- wrapDomEvent (_el_element element) (onEventName Mousedown) (return ())
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]
  attrs <- mapDyn (\x-> singleton "class" $ if x then "sequencerButton activated" else "sequencerButton") isActive
  return isActive
