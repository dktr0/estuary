module Estuary.Widgets.TransformedPattern where


import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.PatternTransformer
import Estuary.Widgets.Generic
import Control.Monad
import Estuary.Widgets.SpecificPattern
import Data.Map
import Data.List

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

{-
trivialTransformedPattern :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
trivialTransformedPattern (TransformedPattern ts p) _ = el "div" $ do
  let firstOnly = if (length ts > 0) then (head ts) else (NoTransformer)
  y <- trivialSoundPattern p never
  x <- trivialPatternTransformer firstOnly never
  z <- combineDyn (\(a,_) (b,_) -> TransformedPattern [a] b) x y
  mapDyn (\a -> (a,never)) z
-}

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

dropdownPatternWidget'::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
dropdownPatternWidget' _ _ = do
  let patMap = fromList $ zip [0..] [sContainerWidget (S Blank) never, nContainerWidget (N Blank) never, panContainerWidget (S Blank) never,crushContainerWidget (S Blank) never]
  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] ["s","n","pan","crush"]
  patternDropDown <- dropdown 0 dropDownMap def
  let ddVal = _dropdown_value patternDropDown
  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> sContainerWidget (S Blank) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn )
  soundPatEv' <- widgetHold (sContainerWidget (S Blank) never) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)
  return $ soundPattern

dropdownPatternWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
dropdownPatternWidget iPattern _ = do
  let paramShowList = ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit", "vowel"] -- Map (Map k func) String
  let patternType = head $ words $ show iPattern
  let initialIndex = maybe (0) id $ Data.List.findIndex (==patternType) paramShowList -- Int of initalFunc
  let patMap = fromList $ zip [0..] builderList
  text $ show patternType
  let initialFunc = maybe (builderList!!0) (id) $ Data.Map.lookup initialIndex patMap
  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] paramShowList
  patternDropDown <- dropdown initialIndex dropDownMap def
  let ddVal = _dropdown_value patternDropDown
  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> sContainerWidget (S Blank) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn )
  soundPatEv' <- widgetHold (initialFunc) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)
  return $ soundPattern
  where
    builderList = Prelude.map (\x-> x never) [doubleContainerWidget (Accelerate $ Atom 0 Once), intContainerWidget (Bandf $ Atom 440 Once),
      doubleContainerWidget (Bandq $ Atom 100 Once),doubleContainerWidget (Begin $ Atom 0 Once),
      intContainerWidget (Coarse $ Atom 0 Once), intContainerWidget (Crush $ Atom 16 Once),
      intContainerWidget (Estuary.Tidal.Types.Cut $ Atom 1 Once), intContainerWidget (Cutoff $ Atom 440 Once),
      doubleContainerWidget (Delay $ Atom 0 Once),doubleContainerWidget (Delayfeedback $ Atom 0 Once),
      doubleContainerWidget (Delaytime $ Atom 0.5 Once), doubleContainerWidget (End $ Atom 1 Once),
      doubleContainerWidget (Gain $ Atom 1 Once), intContainerWidget (Hcutoff $ Atom 440 Once),
      doubleContainerWidget (Hresonance $ Atom 20 Once),
      intContainerWidget (Loop $ Atom 0 Once), intContainerWidget (N $ Atom 0 Once),
      doubleContainerWidget (Pan $ Atom 0.5 Once), doubleContainerWidget (Resonance $ Atom 0.5 Once),
      stringContainerWidget (S Blank), doubleContainerWidget (Shape $ Atom 0.5 Once),
      doubleContainerWidget (Speed $ Atom 1 Once), charContainerWidget (Unit $ Atom 'c' Once), charContainerWidget (Vowel $ Atom 'o' Once)] --, stringContainerWidget (Unit $ Atom "c" Once),stringContainerWidget (Vowel $ Atom "c" Once)]


transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
transformedPatternWidget (TransformedPattern ts p) _ = el "div" $ do
  deleteEvents <- buttonDynAttrs "-" (DeleteMe) (constDyn $ "style"=:"background-color:salmon")
  (soundPattern,events) <- dropdownPatternWidget (p) never >>= splitDyn
  --deleteEvents <- mapDyn (ffilter (==DeleteMe)) events --Dyn Event DeleteMe
  transformer <- parameteredPatternTransformer (f ts) never
  transformedPat <- combineDyn(\(a,_) b-> TransformedPattern [a] b) transformer soundPattern -- Dyn transformedPat
  mapDyn (\a-> (a,deleteEvents)) transformedPat
  where
    f [] = NoTransformer -- sorry again...
    f (x:_) = x
    lookupBuilder x = sContainerWidget


transformedPatternWidget'' :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
transformedPatternWidget'' (TransformedPattern ts p) _ = el "div" $ do
  let patMap = fromList $ zip [0..] [sContainerWidget (S Blank) never, nContainerWidget (S Blank) never, panContainerWidget (S Blank) never,crushContainerWidget (S Blank) never]

  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] ["s","n","pan","crush"] -- Map (Map k func) String
  patternDropDown <- dropdown 0 dropDownMap def
  let ddVal = _dropdown_value patternDropDown

  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> sContainerWidget (S Blank) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn )

  soundPatEv' <- widgetHold (sContainerWidget (S Blank) never) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)


  eventsFromPattern <- liftM (switchPromptlyDyn) $ mapDyn (snd) soundPattern
  let deleteEvents = ffilter (==DeleteMe) eventsFromPattern
  transformer <- parameteredPatternTransformer (f ts) never
  transformedPat <- combineDyn(\(a,_) (b,_)-> TransformedPattern [a] b) transformer soundPattern -- Dyn transformedPat
  mapDyn (\a-> (a,deleteEvents)) transformedPat
  where
    f [] = NoTransformer -- sorry again...
    f (x:_) = x
