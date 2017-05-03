module Estuary.Widgets.TransformedPattern where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.PatternTransformer
import Estuary.Widgets.Generic
import Control.Monad
import Data.Map
import Data.List
import qualified Estuary.Widgets.SpecificPattern as Sp


dropdownPatternWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t ()))
dropdownPatternWidget iPattern _ = do
  let paramShowList = ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"] -- Map (Map k func) String
  let patternType = head $ words $ show iPattern
  let initialIndex = maybe (0) id $ Data.List.findIndex (==patternType) paramShowList -- Int of initalFunc
  let patMap = fromList $ zip [0..] builderList
  let initialFunc = maybe (builderList!!0) (id) $ Data.Map.lookup initialIndex patMap
  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] paramShowList
  patternDropDown <- dropdown initialIndex dropDownMap def
  let ddVal = _dropdown_value patternDropDown
  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> Sp.specificContainer (S Blank) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn )
  soundPatEv' <- widgetHold (initialFunc) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)
  return $ soundPattern
  where
    builderList = Prelude.map (\x-> Sp.specificContainer x never) [(Accelerate $ Atom 0 Inert Once),
      (Bandf $ Atom 440 Inert Once),
      (Bandq $ Atom 10 Inert Once),
      (Begin $ Atom 0 Inert Once),
      (Coarse $ Atom 0 Inert Once),
      (Crush $ Atom 16 Inert Once),
      (Estuary.Tidal.Types.Cut $ Atom 1 Inert Once),
      (Cutoff $ Atom 440 Inert Once),
      (Delay $ Atom 0 Inert Once),
      (Delayfeedback $ Atom 0 Inert Once),
      (Delaytime $ Atom 0.5 Inert Once),
      (End $ Atom 1 Inert Once),
      (Gain $ Atom 1 Inert Once),
      (Hcutoff $ Atom 440 Inert Once),
      (Hresonance $ Atom 20 Inert Once),
      (Loop $ Atom 0 Inert Once),
      (N $ Atom 0 Inert Once),
      (Pan $ Atom 0.5 Inert Once),
      (Resonance $ Atom 0.5 Inert Once),
      (S $ Atom "~" Inert Once),
      (Shape $ Atom 0.5 Inert Once),
      (Speed $ Atom 1 Inert Once),
      (Unit $ Atom 'c' Inert Once),
      (Up $ Atom 0 Inert Once),
      (Vowel $ Atom 'o' Inert Once)] --, stringContainerWidget (Unit $ Atom "c" Once),stringContainerWidget (Vowel $ Atom "c" Once)]


transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t (EditSignal a)))
transformedPatternWidget (TransformedPattern ts p) _ = el "div" $ do
  deleteEvents <- buttonDynAttrs "-" (DeleteMe) (constDyn $ "class"=:"patternAddButton")
  (soundPattern,events) <- dropdownPatternWidget (p) never >>= splitDyn
  --deleteEvents <- mapDyn (ffilter (==DeleteMe)) events --Dyn Event DeleteMe
  transformer <- parameteredPatternTransformer (f ts) never
  transformedPat <- combineDyn(\(a,_) b-> TransformedPattern [a] b) transformer soundPattern -- Dyn transformedPat
  mapDyn (\a-> (a,deleteEvents)) transformedPat
  where
    f [] = NoTransformer -- sorry again...
    f (x:_) = x
    lookupBuilder x = Sp.sContainerWidget


transformedPatternWidget' :: MonadWidget t m => TransformedPattern -> m (Dynamic t TransformedPattern)

transformedPatternWidget' (UntransformedPattern u) = do
  b <- button "transformMe"
  transformedPatternWidget'' (UntransformedPattern u)

transformedPatternWidget'' (UntransformedPattern u) = do
  u' <- specificPatternWidget u
  mapDyn UntransformedPattern u'

transformedPatternWidget' (TransformedPattern t x) = do
  b <- button "transformMe" >>= (   <$)
  transformedPatternWidget'' (TransformedPattern t x)

transformedPatternWidget'' (TransformedPattern t x) = do
  t' <- patternTransformedWidget t
  x' <- transformedPatternWidget' x
  combineDyn TransformedPattern t' x'
