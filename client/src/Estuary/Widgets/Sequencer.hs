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
import Estuary.Widgets.Generic -- for EditSignal... TODO move that


type Sequence a = Map Int (Maybe a,[Bool])

attachIndex:: [a] -> [(Int,a)]
attachIndex l = zip (take (length l) [0..]) l

sequencer ::MonadWidget t m => [(String, [Bool])] -> Event t ([(String,[Bool])]) -> m (Dynamic t ([(String,[Bool])], Event t ([(String,[Bool])]), Event t Hint))
sequencer i update = elClass "table" "sequencer" $ mdo
  let iMap = fromList $ attachIndex i -- Sequence
  let seqLen = maximum $ fmap (length . snd) i
  let updateVals = fmap (fromList . attachIndex . fmap Just) update
  let downstreamEvs = leftmost [updateVals, newRow, deleteEvents] -- Event t (Map Int (Maybe (String,[Bool])))
  widgets <- liftM joinDynThroughMap $ listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow) -- Dyn t (Map Int ((String,[Bool]), Event t ()))
  values <- mapDyn (fmap fst) widgets -- Dyn t (Map Int (String,[Bool]))
  dynEvs <- mapDyn (mergeMap . fmap (snd)) widgets -- Event t (Map Int ())
  let deleteEvents = fmap (Nothing <$) $ switch $ current $ dynEvs
  let updatedVal = fmap elems $ updated values -- Event t (Map Int (String,[Bool]))
  maxKey <- mapDyn (maybe 0 id . maximumMay . keys) $ nubDyn values
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> singleton (k+1) (Just ("",take seqLen $ repeat False))) (current maxKey) plusButton
  mapDyn (\v-> (elems v, updatedVal, never)) values

sequencerRow ::(MonadWidget t m) => (String,[Bool]) -> Event t (String,[Bool]) -> m (Dynamic t ((String,[Bool]), Event t ()))
sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
  let buttonIVals = M.fromList $ attachIndex vals
  deleteMe <- clickableTdClass (constDyn " - ") (constDyn "delete") ()
  rowInput <- el "td" $ growingTextInput $ def & textInputConfig_initialValue .~ iVal
  buttons <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals (fmap (M.fromList . attachIndex . fmap Just . snd) edits) sequencerButton  -- Dyn (Map Int Bool)
  val <- combineDyn (\s b -> (s, elems b)) (_textInput_value rowInput) buttons
  mapDyn (\x->(x,deleteMe)) val

sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t Bool)
sequencerButton pos val edits = mdo
  (element,_) <- elDynAttr' "td" attrs $ return ()
  clickEv <- wrapDomEvent (_el_element element) (onEventName Mousedown) (return ())
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]
  attrs <- mapDyn (\x-> singleton "class" $ if x then "sequencerButtonActived" else "sequencerButtonDeactivated") isActive
  return isActive
