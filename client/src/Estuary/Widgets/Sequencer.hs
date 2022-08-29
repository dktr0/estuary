{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Sequencer where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Control.Monad
import Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Safe.Foldable (maximumMay)
import Text.Read (readMaybe)
import Estuary.Types.Hint
import Estuary.Widgets.Reflex
import Estuary.Widgets.Reflex
import Estuary.Widgets.W


type Sequence = Map Int (Text, [Bool])

attachIndex :: [a] -> [(Int,a)]
attachIndex = zip [0..]

sequencer :: MonadWidget t m => Dynamic t Sequence -> W t m (Variable t Sequence)
sequencer x = variableWidget x sequencer'

sequencer' :: MonadWidget t m
  => Sequence -> Event t Sequence -> m (Event t Sequence)
sequencer' iMap update = elClass "table" "sequencer" $ mdo
  let seqLen = maximum $ fmap (length . snd) $ elems iMap
  let serverDeletes = fmap (Nothing <$) $ attachWith M.difference (current values) update -- for deleted rows
  let updateEvs = mergeWith union [fmap (fmap Just) update, serverDeletes]
  let downstreamEvs = leftmost [updateEvs, newRow, deleteEvents] -- Event t (Map Int (Maybe (Text,[Bool])))
  ourList <- listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow)
    -- Dynamic t (Map Int (Dynamic t ((Text,[Bool]),Event t ())))
  let widgets = joinDynThroughMap ourList -- Dyn t (Map Int ((Text,[Bool]), Event t ()))
    -- Dynamic t (Map Int ((Text,[Bool]),Event t ()))
  let values = fmap (fmap fst) widgets
    -- Dynamic t (Map Int ((Text,[Bool])))
  let dynEvs = fmap (mergeMap . fmap (snd)) widgets -- Event t (Map Int ())
  let deleteEvents = fmap (Nothing <$) $ switch $ current $ dynEvs
  newValues <- holdUniqDyn values
  let updateVal = fmapMaybe id $ mergeWith (\a b->Nothing) [Nothing <$ update, fmap Just $ updated newValues]
  let maxKey = fmap (maybe 0 id . maximumMay . keys) newValues
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> singleton (k+1) (Just ("",Prelude.take seqLen $ repeat False))) (current maxKey) plusButton
  return updateVal
  -- *** TODO should rework the above since some of the above management is unnecessary with Editor approach


-- Event returned is a message to delete that row
sequencerRow ::(MonadWidget t m) => (Text,[Bool]) -> Event t (Text,[Bool]) -> m (Dynamic t ((Text,[Bool]), Event t ()))
sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
  let buttonIVals = M.fromList $ attachIndex vals
  let strUpdate = fmap fst edits
  let buttonUpdates = fmap (M.fromList . attachIndex . fmap Just . snd) edits
  let textInputAttrs = singleton "class" "sequencer-textarea code-font other-borders"
  deleteMe <- elClass "td" "delete" $ dynButton "-" -- clickableTdClass (constDyn " - ") (constDyn "delete") ()
  rowInput <- elClass "td"  "sequencer-textarea code-font primary-color other-borders" $ textInput $ def & textInputConfig_initialValue .~ iVal & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn ("class" =: "sequencer-textarea  code-font primary-color"))
  buttons <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals buttonUpdates sequencerButton  -- Dyn (Map Int Bool)
  let val = (\s b -> (s, elems b)) <$> (_textInput_value rowInput) <*> buttons
  return $ fmap (\x->(x,deleteMe)) val


sequencerButton :: (MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t Bool)
sequencerButton pos val edits = mdo
  (element,_) <- elDynAttr' "td" attrs $ return ()
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Mousedown) (return ())
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]
  let attrs = fmap (\x-> singleton "class" $ if x then "sequencerButton sequencerButton-activated primary-color other-borders" else "sequencerButton other-borders") isActive
  return isActive
