module Estuary.Widgets.TransformedPattern where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.PatternTransformer
import Estuary.Widgets.Generic
import Control.Monad
import Data.Map (singleton)

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
transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t GenericSignal))
transformedPatternWidget (TransformedPattern ts p) _ = el "div" $ do
  soundPat <- trivialSoundPattern p never
  eventsFromPattern <- liftM (switchPromptlyDyn) $ mapDyn (snd) soundPat
  let deleteEvents = ffilter (==DeleteMe) eventsFromPattern
  transformer <- parameteredPatternTransformer (f ts) never
  transformedPat <- combineDyn(\(a,_) (b,_)-> TransformedPattern [a] b) transformer soundPat -- Dyn transformedPat
  mapDyn (\a-> (a,deleteEvents)) transformedPat
  where
    f [] = NoTransformer -- sorry again...
    f (x:_) = x
