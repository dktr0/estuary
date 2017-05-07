
module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.StackedPatterns
import Estuary.Widgets.PatternChain as P
import Estuary.Widgets.GeneralPattern as G -- for testing the Refactor of general container
import Estuary.Widgets.Text
import Control.Monad (liftM)
import Sound.Tidal.Context (ParamPattern)
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.WebDirt
import Data.Map
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  wd <- webDirt
  stream <- webDirtStream wd
  mainWidget $ do
    divClass "estuary" $ do
      newPage <- header
      divClass "page" $ do
        let firstPage = snd (pages!!0)
        let newPage' = fmap (snd . (pages !!)) newPage
        w <- widgetHold firstPage newPage'
        p <- liftM (joinDyn) $ mapDyn (fst) w
        h <- liftM (switchPromptlyDyn) $ mapDyn (snd) w
        let patternEval = updated p
        performEvent_ $ fmap (liftIO . (doHint wd)) h
        performEvent_ $ fmap (liftIO . stream) patternEval

header :: (MonadWidget t m) => m (Event t Int)
header = divClass "header" $ do
  divClass "logo" $ text "estuary (a work in progress)"
  divClass "webDirt" $ text " "
  newPageIndex <- divClass "pageMenu" $ do
    let pageNames = Prelude.map (fst) pages
    let pageList = zipWith (\x y -> (y,x)) pageNames ([0..]::[Int])
    let pageMap = constDyn $ fromList pageList
    menu <- dropdown 0 pageMap def
    return $ _dropdown_change menu
  divClass "hintArea" $ text " "
  return newPageIndex

widgetToPage :: (MonadWidget t m,ParamPatternable p) => m (Dynamic t (p,a,Event t Hint)) -> m (Dynamic t ParamPattern,Event t Hint)
widgetToPage w = do
  x <- w
  p <- mapDyn (\(a,_,_) -> toParamPattern a) x
  h <- liftM (switchPromptlyDyn) $ mapDyn (\(_,_,a) -> a) x
  return (p,h)

-- pages :: MonadWidget t m => [(String,m (Dynamic t ParamPattern,Event t Hint))]
pages = [
  ("Simple Fixed (s,vowel,up)",widgetToPage $ P.simpleFixedInterface EmptyTransformedPattern never),
  ("Text-Only Fixed (s,n,up,vowel)",widgetToPage $ textInterface EmptyTransformedPattern never),
  ("Two Stacked Patterns with Liveness controls",widgetToPage $ twoStackedPatterns)
  ]


examplePage :: MonadWidget t m => Event t (Map Int (Either TransformedPattern String))
  -> m
    (Dynamic t (Map Int (Either TransformedPattern String)), -- values for local use
     Event t (Map Int (Either TransformedPattern String)), -- edit events for broadcast
     Event t Hint) -- hint events for local use

examplePage = do
  -- let deltaA = fmapMaybe (either Just (const Nothing)) $ fmapMaybe (lookup 1) deltasDown
  -- let deltaB = fmapMaybe (either (const Nothing) Just) $ fmapMaybe (lookup 2) deltasDown
  aValue <- trivialTransformedPatternWidget
  bValue <- textWidget "(blank text to start)"
  let aValue' = mapDyn (singleton 1 . Left) aValue
  let bValue' = mapDyn (singleton 2 . Right) bValue
  values <- combineDyn (union) aValue' bValue'
  let aDeltaUp = fmap (singleton 1 . Left) $ updated aValue -- note: this is not a viable long-term solution
  let bDeltaUp = fmap (singleton 2 . Right) $ updated bValue
  let deltasUp = mergeWith union [aDeltaUp,bDeltaUp]
  -- let hintsUp = leftmost [aHints,bHints]
  let hintsUp = never
  return (values,deltasUp,hintsUp)

trivialPatternA = UntransformedPattern (S (Atom "bd" Inert Once))

trivialPatternB = UntransformedPattern (S (Atom "cp" Inert Once))

trivialTransformedPatternWidget :: MonadWidget t m => m (Dynamic t TransformedPattern)
trivialTransformedPatternWidget = do
  a <- liftM (trivialPatternA <$) $ button "trivialA"
  b <- liftM (trivialPatternB <$) $ button "trivialB"
  holdDyn EmptyTransformedPattern $ leftMost [a,b]

{-
topLevelTransformedPatternWidget :: MonadWidget t m =>
  Event t TransformedPattern -> -- deltas from network (must not re-propagate as edit events!)
  m (
    Dynamic t TransformedPattern, -- value for local WebDirt playback
    Event t TransformedPattern, -- deltas to network (not based on events received from network!)
    Event t Hint, -- hints (currently for WebDirt sample loading only)
  )

topLevelTransformedPatternWidget updateEvent = do
  w <- widgetHold (midLevelTransformedPatternWidget EmptyTransformedPattern) (fmap midLevelTransformedPatternWidget updateEvent)
  x <- mapDyn (\(a,_,_) -> a) w
  y <- mapDyn (\(_,a,_) -> a) w
  z <- mapDyn (\(_,_,a) -> a) w
  let x' = joinDyn x
  let y' = switchPromptlyDyn y
  let z' = switchPromptlyDyn z
  return (x',y',z')

midLevelTransformedPatternWidget :: MonadWidget t m =>
  TransformedPattern -> m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
-- i.e. adapting from what we need at higher level to recursively-structured transformedPatternWidget
-}

textWidget :: MonadWidget t m => String -> m (Dynamic t String)
textWidget i = do
  x <- button "eval"
  y <- textInput $ def & textInputConfig_initialValue .~ i
  let y' = updated $ _textInput_value y
  return y'

--main :: IO ()
--main = mainWidget $ do
--  (values,deltasUp,hints) <- examplePage
--  text "Values:"
--  display values
--  text "DeltasUp:"
--  holdDyn "" $ fmap show deltasUp >>= display
--  text "Hints:"
--  holdDyn "" $ fmap show hints >>= display
