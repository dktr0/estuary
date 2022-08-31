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
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Hint
import Estuary.Widgets.AudioMap
import Estuary.Widgets.StopWatchExplorations
import Estuary.Widgets.Notepad
import Estuary.Widgets.CalendarEvent
import Estuary.Widgets.DataVisualisers
import Estuary.Widgets.Chat



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

viewWidget (LabelView z) = zoneWidget z "" maybeLabelText LabelText labelEditor

viewWidget (StructureView z) = zoneWidget z EmptyTransformedPattern maybeTidalStructure TidalStructure structureEditor

viewWidget (CodeView z rows styles) = do
  whenever <- liftIO $ getCurrentTime
  errorDyn <- fmap (IntMap.lookup z) <$> errors
  zoneWidget z (Live (UnspecifiedNotation,"",whenever) L3) maybeTextProgram TextProgram (textProgramEditor styles rows errorDyn)

viewWidget (SequenceView z) = zoneWidget z defaultValue maybeSequence Sequence sequencer
  where defaultValue = Map.singleton 0 ("",replicate 8 False)

viewWidget EnsembleStatusView = ensembleStatusWidget

viewWidget (RouletteView z rows) = zoneWidget z [] maybeRoulette Roulette (rouletteWidget rows)

viewWidget (CalendarEventView z) = do
  today <- liftIO getZonedTime
  let defaultValue = IntMap.singleton 0 (CalendarEvent "Add a title" (CalendarTime today (Recurrence Once today)))
  zoneWidget z defaultValue maybeCalendarEvents CalendarEvs calendarEventWidget

viewWidget (CountDownView z) = zoneWidget z (Holding 60) maybeTimerDownState CountDown countDownWidget

viewWidget (SandClockView z) = zoneWidget z (Holding 60) maybeTimerDownState CountDown sandClockWidget

viewWidget (StopWatchView z) = zoneWidget z Cleared maybeTimerUpState StopWatch stopWatchWidget

viewWidget (SeeTimeView z) = zoneWidget z (Cyclic 0) maybeSeeTime SeeTime visualiseTempoWidget

viewWidget (NotePadView z) = zoneWidget z (0,Seq.fromList[("Title","Content")]) maybeNotePad NotePad notePadWidget

viewWidget (ChatView z) = zoneWidget z [] maybeSpecChat SpecChat chatWidget


viewWidget TempoView = return () {- do -- disactivating TempoView - noone uses it anyway...
  initialTempo <- tempo >>= (sample . current)
  tempoDelta <- holdDyn initialTempo $ fmapMaybe lastTempoChange er
  tempoE <- tempoWidget tempoDelta
  return $ fmap WriteTempo tempoE -}

viewWidget (Snippet z b n t) = do
  let c = if b then "example code-font" else "snippet code-font"
  b <- clickableDiv c $ text t
  bTime <- performEvent $ fmap (liftIO . const getCurrentTime) b
  hint $ fmap (\et -> ZoneHint z (TextProgram (Live (n,t,et) L3))) bTime

viewWidget AudioMapView = audioMapWidget

viewWidget (LoadView 0) = graphVisionWidget

viewWidget (LoadView 1) = vintageVisionWidget

viewWidget (LoadView 2) = concentricCircleVisionWidget

viewWidget (IFrame url) = do
  let attrs = Map.fromList [("src",url), ("style","height:100%"), ("allow","microphone *")]
  elAttr "iframe" attrs $ return ()


zoneWidget :: (Monad m, MonadSample t m, Reflex t, MonadHold t m, MonadFix m, Eq a)
  => Int -> a -> (Definition -> Maybe a) -> (a -> Definition)
  -> (Dynamic t a -> W t m (Variable t a))
  -> W t m ()
zoneWidget zoneNumber defaultA defToMaybeA aToDef anEditingWidget = do
  let getA = maybe defaultA (maybe defaultA id . defToMaybeA) . IntMap.lookup zoneNumber
  zs <- zones
  dynA <- holdUniqDyn $ fmap getA zs -- note: when zones are streamed separately, holdUniqDyn will not be necessary
  varA <- anEditingWidget dynA
  hint $ (ZoneHint zoneNumber . aToDef) <$> localEdits varA
