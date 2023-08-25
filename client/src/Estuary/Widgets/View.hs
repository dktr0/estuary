{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.View where

import Reflex
import Reflex.Dom hiding (Link)
import Control.Monad.Fix (MonadFix)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import TextShow
import Data.Time
import Data.Bool
import qualified Data.Sequence as Seq
import GHCJS.DOM.EventM

import Estuary.Types.Live
import Estuary.Types.CodeWidgetOptions
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Tidal.Types (TransformedPattern(..))
import Estuary.Types.TextNotation
import Estuary.Types.TidalParser
import Estuary.Types.Tempo
import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Sequencer
import Estuary.Widgets.Roulette
import Estuary.Widgets.EnsembleStatus
import Estuary.Widgets.Tempo
import Estuary.Widgets.Timer
import Estuary.Types.Hint
import Estuary.Widgets.AudioMap
import Estuary.Widgets.StopWatchExplorations
import Estuary.Widgets.Notepad
import Estuary.Widgets.CalendarEvent
import Estuary.Widgets.TestMap
import Estuary.Widgets.DataVisualisers
import Estuary.Widgets.Chat
import Estuary.Types.Request
import Estuary.Widgets.TapTempo



attrsColp :: Bool -> Map.Map T.Text T.Text
attrsColp b = "class" =: "collapsableView" <> "style" =: ("display: " <> showDiv b)
  where
    showDiv True  = "block"
    showDiv False = "none"

viewsContainerCollaps :: (Monad m, Reflex t, DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m) => m a -> m a
viewsContainerCollaps x = mdo
  dynBool <- toggle True evClick
  evClick <- dynButton dynOpenClose
  let dynOpenClose = (bool "+" "-") <$> dynBool -- Dyn Text
  let dynAttr = attrsColp <$> dynBool -- Dyn Value
  elDynAttr "div" dynAttr x


viewWidget :: MonadWidget t m => View -> W t m ()

viewWidget EmptyView = return ()

viewWidget (Div c vs) = divClass c $ mapM_ viewWidget vs

viewWidget (Views vs) = viewWidget (Div "views" vs)

viewWidget (Columns vs) = viewWidget (Div "columns" vs)

viewWidget (Rows vs) = viewWidget (Div "rows" vs)

viewWidget (BorderDiv vs) = viewWidget (Div "borderDiv" vs)

viewWidget (Paragraph vs) = viewWidget (Div "paragraph code-font" vs)

viewWidget (Link url vs) = elAttr "a" ("href" =: url) $ viewWidget (Views vs)

viewWidget (BulletPoints vs) = el "ul" $ forM_ vs $ \v -> el "li" $ viewWidget v

viewWidget (GridView c r vs) = viewsContainer $ mapM_ (\v -> divClass "gridChild" $ viewWidget v) vs
  where
    viewsContainer x = elAttr "div" ("class" =: "gridView" <> "style" =: (setColumnsAndRows) ) $ x
    defineNumRowsOrColumns n = replicate n $ showt ((100.0 :: Double) / (fromIntegral n)) <> "%"
    setNumColumns =  "grid-template-columns: " <> (T.intercalate " " $ defineNumRowsOrColumns c) <> ";"
    setNumRows =  "grid-template-rows: " <> (T.intercalate " " $ defineNumRowsOrColumns r) <> ";"
    setColumnsAndRows  = setNumColumns <> setNumRows

viewWidget (CollapsableView v) = viewsContainerCollaps $ divClass "gridChild" $ viewWidget v

viewWidget (Text t) = translatableText t >>= dynText

viewWidget (LabelView z) = zoneWidget False z "" maybeLabelText LabelText labelEditor

viewWidget (StructureView z) = zoneWidget True z EmptyTransformedPattern maybeTidalStructure TidalStructure structureEditor

viewWidget (CodeView zoneNumber rows styles) = do
  whenever <- liftIO $ getCurrentTime
  errorDyn <- fmap (IntMap.lookup zoneNumber) <$> errors
  -- zoneWidget True z (Live (UnspecifiedNotation,"",whenever) L3) maybeTextProgram TextProgram (textProgramEditor styles rows errorDyn)
  zs <- zones
  let defaultLiveTextProgram = (Live (UnspecifiedNotation,"",whenever) L3)
  let getLiveTextProgram = maybe defaultLiveTextProgram (maybe defaultLiveTextProgram id . maybeTextProgram) . IntMap.lookup zoneNumber
  dynLiveTextProgram <- holdUniqDyn $ fmap getLiveTextProgram zs
  varA <- textProgramEditor styles rows errorDyn dynLiveTextProgram -- :: W t m (Variable t (Live TextProgram))
  let reqF x = WriteZone zoneNumber (TextProgram x) (not $ isEdited x)
  request $ reqF <$> localEdits varA

viewWidget (SequenceView z) = zoneWidget True z defaultValue maybeSequence Sequence sequencer
  where defaultValue = Map.singleton 0 ("",replicate 8 False)

viewWidget EnsembleStatusView = ensembleStatusWidget

viewWidget (RouletteView z rows) = zoneWidget False z [] maybeRoulette Roulette (rouletteWidget rows)

viewWidget (CalendarEventView z) = do
  today <- liftIO getZonedTime
  let defaultValue = Map.singleton 0 (CalendarEvent "" (CalendarTime today (Recurrence Once today)))
  zoneWidget False z defaultValue maybeCalendarEvents CalendarEvs calendarEventWidget

viewWidget (TestView z) = do
  today <- liftIO getZonedTime
  let defaultValue = IntMap.singleton 0 "a testMap"
  zoneWidget False z defaultValue maybeTestEvent Test testMapWidget


viewWidget (CountDownView z) = zoneWidget False z (Holding 60) maybeTimerDownState CountDown countDownWidget

viewWidget (SandClockView z) = zoneWidget False z (Holding 60) maybeTimerDownState CountDown sandClockWidget

viewWidget (StopWatchView z) = zoneWidget False z Cleared maybeTimerUpState StopWatch stopWatchWidget

-- viewWidget er (SeeTimeView z) = zoneWidget z (Tv 0 4 0) maybeSeeTime SeeTime er visualiseTempoWidget

viewWidget (TimerView z) = zoneWidget False z (Timer 0 [("a",5),("b",7),("c",3)] Halted True Cycles) maybeTimer TimerDef timerWidget

viewWidget (SeeTimeView z) = zoneWidget False z (Tv 0 4 0) maybeSeeTime SeeTime visualiseTempoWidget

viewWidget (NotePadView z) = zoneWidget False z (0,Seq.fromList[("Title","Content")]) maybeNotePad NotePad notePadWidget

viewWidget (ChatView z) = zoneWidget False z [] maybeSpecChat SpecChat chatWidget


viewWidget TempoView = return () {- do -- disactivating TempoView - noone uses it anyway...
  initialTempo <- tempo >>= (sample . current)
  tempoDelta <- holdDyn initialTempo $ fmapMaybe lastTempoChange er
  tempoE <- tempoWidget tempoDelta
  return $ fmap WriteTempo tempoE -}
  
viewWidget TapTempoView = tapTempoWidget

viewWidget (Snippet z b n t) = do
  let c = if b then "example code-font" else "snippet code-font"
  b <- clickableDiv c $ text t
  bTime <- performEvent $ fmap (liftIO . const getCurrentTime) b
  request $ fmap (\et -> WriteZone z (TextProgram (Live (n,t,et) L3)) True) bTime

viewWidget AudioMapView = audioMapWidget

viewWidget (LoadView 0) = graphVisionWidget

viewWidget (LoadView 1) = vintageVisionWidget

viewWidget (LoadView 2) = concentricCircleVisionWidget

viewWidget (IFrame url) = do
  let attrs = Map.fromList [("src",url), ("style","height:100%"), ("allow","microphone *")]
  elAttr "iframe" attrs $ return ()


zoneWidget :: (Monad m, MonadSample t m, Reflex t, MonadHold t m, MonadFix m, Eq a)
  => Bool -> Int -> a -> (Definition -> Maybe a) -> (a -> Definition)
  -> (Dynamic t a -> W t m (Variable t a))
  -> W t m ()
zoneWidget changesRender zoneNumber defaultA defToMaybeA aToDef anEditingWidget = do
  let getA = maybe defaultA (maybe defaultA id . defToMaybeA) . IntMap.lookup zoneNumber
  zs <- zones
  dynA <- holdUniqDyn $ fmap getA zs -- note: when zones are streamed separately, holdUniqDyn will not be necessary
  varA <- anEditingWidget dynA
  request $ ((\x -> WriteZone zoneNumber x changesRender) . aToDef) <$> localEdits varA
