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
-- data SpecificPattern = S (GeneralPattern SampleName) | N (GeneralPattern Int) | Sound (GeneralPattern Sample) | Pan (GeneralPattern Double) deriving (Eq)


trivialSoundPattern :: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
trivialSoundPattern iValue _ = el "div" $ do
  elAttr "div" (singleton "style" "display: inline;") $ text "s "
  x <- clickableDiv' "bd cp " $ sPatternFromList ["bd","cp"]
  y <- clickableDiv' "arpy*4 " $ sPatternFromList ["arpy","arpy","arpy","arpy"]
  z <- clickableDiv' "~ arp " $ S (Group [Blank,Atom "arp" Once] Once)
  pattern <- holdDyn iValue $ leftmost [x,y,z]
  deleteMe <- button' "-" DeleteMe
  mapDyn (\a -> (a,deleteMe)) pattern


---  data PatternTransformer = NoTransformer | Rev | Slow Rational | Density Rational | Degrade | DegradeBy Double | Every Int PatternTransformer | Brak | Jux PatternTransformer deriving (Ord,Eq)

-- TransformedPattern [PatternTransformer] SoundPattern
transformedPatternWidget' :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
transformedPatternWidget' (TransformedPattern ts p) _ = el "div" $ do
  soundPat <- trivialSoundPattern p never
  eventsFromPattern <- liftM (switchPromptlyDyn) $ mapDyn (snd) soundPat
  let deleteEvents = ffilter (==DeleteMe) eventsFromPattern
  transformer <- parameteredPatternTransformer (f ts) never
  transformedPat <- combineDyn(\(a,_) (b,_)-> TransformedPattern [a] b) transformer soundPat -- Dyn transformedPat
  mapDyn (\a-> (a,deleteEvents)) transformedPat
  where
    f [] = NoTransformer -- sorry again...
    f (x:_) = x


dropdownPatternWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
dropdownPatternWidget iPattern _ = do
  let paramShowList = ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"] -- Map (Map k func) String
  let patternType = head $ words $ show iPattern
  let initialIndex = maybe (0) id $ Data.List.findIndex (==patternType) paramShowList -- Int of initalFunc
  let patMap = fromList $ zip [0..] builderList
  let initialFunc = maybe (builderList!!0) (id) $ Data.Map.lookup initialIndex patMap
  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] paramShowList
  patternDropDown <- dropdown initialIndex dropDownMap def
  let ddVal = _dropdown_value patternDropDown
  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> Sp.sContainerWidget (S Blank) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn )
  soundPatEv' <- widgetHold (initialFunc) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)
  return $ soundPattern
  where
    builderList = Prelude.map (\x-> Sp.specificContainer x never) [(Accelerate $ Atom 0 Once), 
      (Bandf $ Atom 440 Once),
      (Bandq $ Atom 10 Once),
      (Begin $ Atom 0 Once),
      (Coarse $ Atom 0 Once), 
      (Crush $ Atom 16 Once),
      (Estuary.Tidal.Types.Cut $ Atom 1 Once), 
      (Cutoff $ Atom 440 Once),
      (Delay $ Atom 0 Once),
      (Delayfeedback $ Atom 0 Once),
      (Delaytime $ Atom 0.5 Once), 
      (End $ Atom 1 Once),
      (Gain $ Atom 1 Once), 
      (Hcutoff $ Atom 440 Once),
      (Hresonance $ Atom 20 Once),
      (Loop $ Atom 0 Once), 
      (N $ Atom 0 Once),
      (Pan $ Atom 0.5 Once), 
      (Resonance $ Atom 0.5 Once), 
      (S $ Atom "~" Once), 
      (Shape $ Atom 0.5 Once),
      (Speed $ Atom 1 Once), 
      (Unit $ Atom 'c' Once), 
      (Up $ Atom 0 Once),
      (Vowel $ Atom 'o' Once)] --, stringContainerWidget (Unit $ Atom "c" Once),stringContainerWidget (Vowel $ Atom "c" Once)]


--dropdownPatternWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
--dropdownPatternWidget iPattern _ = do
--  let paramShowList = ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"] -- Map (Map k func) String
--  let patternType = head $ words $ show iPattern
--  let initialIndex = maybe (0) id $ Data.List.findIndex (==patternType) paramShowList -- Int of initalFunc
--  let patMap = fromList $ zip [0..] builderList
--  let initialFunc = maybe (builderList!!0) (id) $ Data.Map.lookup initialIndex patMap
--  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] paramShowList
--  patternDropDown <- dropdown initialIndex dropDownMap def
--  let ddVal = _dropdown_value patternDropDown
--  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> Sp.sContainerWidget (S Blank) never) ddVal  --Dynamic (m(dynamic spec,event t))
--  let soundPatEv = updated soundPat -- Event(Dyn )
--  soundPatEv' <- widgetHold (initialFunc) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
--  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)
--  return $ soundPattern
--  where
--    builderList = Prelude.map (\x-> x never) [Sp.specificDoubleContainer (Accelerate $ Atom 0 Once), 
--      Sp.specificContainer (Bandf $ Atom 440 Once),
--      Sp.specificContainer (Bandq $ Atom 10 Once),
--      Sp.specificContainer (Begin $ Atom 0 Once),
--      Sp.specificContainer (Coarse $ Atom 0 Once), 
--      Sp.specificContainer (Crush $ Atom 16 Once),
--      Sp.specificContainer (Estuary.Tidal.Types.Cut $ Atom 1 Once), 
--      Sp.specificContainer (Cutoff $ Atom 440 Once),
--      Sp.specificContainer (Delay $ Atom 0 Once),
--      Sp.specificContainer (Delayfeedback $ Atom 0 Once),
--      Sp.specificContainer (Delaytime $ Atom 0.5 Once), 
--      Sp.specificDoubleContainer (End $ Atom 1 Once),
--      Sp.specificDoubleContainer (Gain $ Atom 1 Once), 
--      Sp.specificIntContainer (Hcutoff $ Atom 440 Once),
--      Sp.specificDoubleContainer (Hresonance $ Atom 20 Once),
--      Sp.intContainerWidget (Loop $ Atom 0 Once), 
--      Sp.specificIntContainer (N $ Atom 0 Once),
--      Sp.specificDoubleContainer (Pan $ Atom 0.5 Once), 
--      Sp.specificDoubleContainer (Resonance $ Atom 0.5 Once),
--      Sp.specificStringContainer (S Blank), 
--      Sp.specificDoubleContainer (Shape $ Atom 0.5 Once),
--      Sp.specificDoubleContainer (Speed $ Atom 1 Once), 
--      Sp.charContainerWidget (Unit $ Atom 'c' Once), 
--      Sp.specificDoubleContainer (Up $ Atom 0 Once),
--      Sp.charContainerWidget (Vowel $ Atom 'o' Once)] --, stringContainerWidget (Unit $ Atom "c" Once),stringContainerWidget (Vowel $ Atom "c" Once)]


dropdownPatternTextWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
dropdownPatternTextWidget iPattern _ = do
  let paramShowList = ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"] -- Map (Map k func) String
  let patternType = head $ words $ show iPattern
  let initialIndex = maybe (0) id $ Data.List.findIndex (==patternType) paramShowList -- Int of initalFunc
  let patMap = fromList $ zip [0..] builderList
  let initialFunc = maybe (builderList!!0) (id) $ Data.Map.lookup initialIndex patMap
  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] paramShowList
  patternDropDown <- dropdown initialIndex dropDownMap def
  let ddVal = _dropdown_value patternDropDown
  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> Sp.sContainerWidget (S Blank) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn )
  soundPatEv' <- widgetHold (initialFunc) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)
  return $ soundPattern
  where
    builderList = Prelude.map (\x-> x never) [Sp.specificContainer (Accelerate $ Atom 0 Once), 
      Sp.intContainerWidget (Bandf $ Atom 440 Once),
      Sp.specificContainer (Bandq $ Atom 10 Once),
      Sp.specificContainer (Begin $ Atom 0 Once),
      Sp.intContainerWidget (Coarse $ Atom 0 Once), 
      Sp.intContainerWidget (Crush $ Atom 16 Once),
      Sp.intContainerWidget (Estuary.Tidal.Types.Cut $ Atom 1 Once), 
      Sp.intContainerWidget (Cutoff $ Atom 440 Once),
      Sp.specificContainer (Delay $ Atom 0 Once),
      Sp.specificContainer (Delayfeedback $ Atom 0 Once),
      Sp.specificContainer (Delaytime $ Atom 0.5 Once), 
      Sp.specificContainer (End $ Atom 1 Once),
      Sp.specificContainer (Gain $ Atom 1 Once), 
      Sp.intContainerWidget (Hcutoff $ Atom 440 Once),
      Sp.specificContainer (Hresonance $ Atom 20 Once),
      Sp.intContainerWidget (Loop $ Atom 0 Once), 
      Sp.intContainerWidget (N $ Atom 0 Once),
      Sp.specificContainer (Pan $ Atom 0.5 Once), 
      Sp.specificContainer (Resonance $ Atom 0.5 Once),
      Sp.specificContainer (S $ Atom "~" Once), 
      Sp.specificContainer (Shape $ Atom 0.5 Once),
      Sp.specificContainer (Speed $ Atom 1 Once), 
      Sp.charContainerWidget (Unit $ Atom 'c' Once), 
      Sp.specificContainer (Up $ Atom 0 Once), 
      Sp.charContainerWidget (Vowel $ Atom 'o' Once)] --, stringContainerWidget (Unit $ Atom "c" Once),stringContainerWidget (Vowel $ Atom "c" Once)]



transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
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

transformedPatternTextWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
transformedPatternTextWidget (TransformedPattern ts p) _ = el "div" $ do
  deleteEvents <- buttonDynAttrs "-" (DeleteMe) (constDyn $ "class"=:"patternAddButton")
  (soundPattern,events) <- dropdownPatternTextWidget (p) never >>= splitDyn
  --deleteEvents <- mapDyn (ffilter (==DeleteMe)) events --Dyn Event DeleteMe
  transformer <- parameteredPatternTransformer (f ts) never
  transformedPat <- combineDyn(\(a,_) b-> TransformedPattern [a] b) transformer soundPattern -- Dyn transformedPat
  mapDyn (\a-> (a,deleteEvents)) transformedPat
  where
    f [] = NoTransformer -- sorry again...
    f (x:_) = x
    lookupBuilder x = Sp.sContainerWidget



