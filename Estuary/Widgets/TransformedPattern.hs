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


-- -- soundPat:: (Dynamic t (SpecificPattern, Event t GenericSignal))
--
-- transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
-- transformedPatternWidget (TransformedPattern ts p) _ = el "div" $ do
--   let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] ["s","n","pan","crush"] -- Map (Map k func) String
--   patternDropDown <- dropdown 0 dropDownMap def
--   let widgetSelector = _dropdown_value patternDropDown -- Dynamic map k (Dyn (specific, ev))
--   pattern <- forDyn widgetSelector (lookupBuilder)
--

transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
transformedPatternWidget (TransformedPattern ts p) _ = el "div" $ do
  let patMap = fromList $ zip [0..] [sContainerWidget (S Blank) never, nContainerWidget (S Blank) never, panContainerWidget (S Blank) never,crushContainerWidget (S Blank) never]

  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] ["s","n","pan","crush"] -- Map (Map k func) String
  patternDropDown <- dropdown 0 dropDownMap def
  let ddVal = _dropdown_value patternDropDown -- Dynamic map k (Dyn (specific, ev))

  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> sContainerWidget (S Blank) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn )

--[W]   widgetHold :: m a ->   Event (m a) -> m (Dynamic a)
--[W]   widgetHold :: m (Dynamic(spec, Event Gen)) ->   Event (m (Dynamic(spec, Event Gen))) -> m (Dynamic (Dynamic(spec, Event Gen)))
  soundPatEv' <- widgetHold (sContainerWidget (S Blank) never) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)

  --let soundPat''' = joinDyn soundPat''

  eventsFromPattern <- liftM (switchPromptlyDyn) $ mapDyn (snd) soundPattern
  let deleteEvents = ffilter (==DeleteMe) eventsFromPattern
  transformer <- parameteredPatternTransformer (f ts) never
  transformedPat <- combineDyn(\(a,_) (b,_)-> TransformedPattern [a] b) transformer soundPattern -- Dyn transformedPat
  mapDyn (\a-> (a,deleteEvents)) transformedPat
  where
    f [] = NoTransformer -- sorry again...
    f (x:_) = x
    lookupBuilder x = sContainerWidget
    --lookupBuilder _ = nContainerWidget (S Blank) never
    --builders = [sContainerWidget (S Blank) never, nContainerWidget (S Blank) never, panContainerWidget (S Blank) never,crushContainerWidget (S Blank) never]!!i
