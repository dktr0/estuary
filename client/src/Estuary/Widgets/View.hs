{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.View where

import Reflex
import Reflex.Dom hiding (Link)
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

viewsContainerCollaps :: MonadWidget t m => W t m (Event t EnsembleRequest) -> W t m (Event t EnsembleRequest)
viewsContainerCollaps x = mdo
  dynBool <- toggle True evClick
  evClick <- dynButton dynOpenClose
  let dynOpenClose = (bool "+" "-") <$> dynBool -- Dyn Text
  let dynAttr = attrsColp <$> dynBool -- Dyn Value
  elDynAttr "div" dynAttr x


viewWidget :: MonadWidget t m => Event t [EnsembleResponse] -> View -> W t m (Event t EnsembleRequest)

viewWidget er EmptyView = return never

viewWidget er (Div c vs) = divClass c $ liftM leftmost $ mapM (viewWidget er) vs

viewWidget er (Views vs) = viewWidget er (Div "views" vs)

viewWidget er (Columns vs) = viewWidget er (Div "columns" vs)

viewWidget er (Rows vs) = viewWidget er (Div "rows" vs)

viewWidget er (BorderDiv vs) = viewWidget er (Div "borderDiv" vs)

viewWidget er (Paragraph vs) = viewWidget er (Div "paragraph code-font" vs)

viewWidget er (Link url vs) = elAttr "a" ("href" =: url) $ viewWidget er (Views vs)

viewWidget er (BulletPoints vs) = el "ul" $ do
  rs <- forM vs $ \v -> el "li" $ viewWidget er v
  return $ leftmost rs

viewWidget er (GridView c r vs) = viewsContainer $ liftM leftmost $ mapM (\v -> divClass "gridChild" $ viewWidget er v) vs
  where
    viewsContainer x = elAttr "div" ("class" =: "gridView" <> "style" =: (setColumnsAndRows) ) $ x
    defineNumRowsOrColumns n = replicate n $ showt ((100.0 :: Double) / (fromIntegral n)) <> "%"
    setNumColumns =  "grid-template-columns: " <> (T.intercalate " " $ defineNumRowsOrColumns c) <> ";"
    setNumRows =  "grid-template-rows: " <> (T.intercalate " " $ defineNumRowsOrColumns r) <> ";"
    setColumnsAndRows  = setNumColumns <> setNumRows


viewWidget er (CollapsableView v) = viewsContainerCollaps $ divClass "gridChild" $ viewWidget er v

viewWidget _ (Text t) = translatableText t >>= dynText >> return never

viewWidget er (LabelView z) = zoneWidget z "" maybeLabelText LabelText er labelEditor

viewWidget er (StructureView z) = zoneWidget z EmptyTransformedPattern maybeTidalStructure TidalStructure er structureEditor

viewWidget er (CodeView z rows styles) = do
  whenever <- liftIO $ getCurrentTime
  errorDyn <- fmap (IntMap.lookup z) <$> errors
  zoneWidget z (Live (UnspecifiedNotation,"",whenever) L3) maybeTextProgram TextProgram er (textProgramEditor styles rows errorDyn)

viewWidget er (SequenceView z) = zoneWidget z defaultValue maybeSequence Sequence er sequencer
  where defaultValue = Map.singleton 0 ("",replicate 8 False)

viewWidget er EnsembleStatusView = ensembleStatusWidget

viewWidget er (RouletteView z rows) = zoneWidget z [] maybeRoulette Roulette er (rouletteWidget rows)

viewWidget er (CalendarEventView z) = do
  today <- liftIO getZonedTime
  let defaultValue = IntMap.singleton 0 (CalendarEvent "Add a title" (CalendarTime today (Recurrence Once today)))
  zoneWidget z defaultValue maybeCalendarEvents CalendarEvs er calendarEventWidget

viewWidget er (CountDownView z) = zoneWidget z (Holding 60) maybeTimerDownState CountDown er countDownWidget

viewWidget er (SandClockView z) = zoneWidget z (Holding 60) maybeTimerDownState CountDown er sandClockWidget

viewWidget er (StopWatchView z) = zoneWidget z Cleared maybeTimerUpState StopWatch er stopWatchWidget

viewWidget er (SeeTimeView z) = zoneWidget z (Cyclic 0) maybeSeeTime SeeTime er visualiseTempoWidget

viewWidget er (NotePadView z) = zoneWidget z (0,Seq.fromList[("Title","Content")]) maybeNotePad NotePad er notePadWidget

viewWidget er (ChatView z) = zoneWidget z [] maybeSpecChat SpecChat er chatWidget


viewWidget er TempoView = do
  initialTempo <- tempo >>= (sample . current)
  tempoDelta <- holdDyn initialTempo $ fmapMaybe lastTempoChange er
  tempoE <- tempoWidget tempoDelta
  return $ fmap WriteTempo tempoE

viewWidget _ (Snippet z b n t) = do
  b <- clickableDiv (snippetOrExample b) $ text t
  bTime <- performEvent $ fmap (liftIO . const getCurrentTime) b
  hint $ fmap (\et -> ZoneHint z (TextProgram (Live (n,t,et) L3))) bTime
  return never

viewWidget _ AudioMapView = do
  audioMapWidget
  return never

viewWidget _ (LoadView 0) = do
  graphVisionWidget
  return never

viewWidget _ (LoadView 1) = do
  vintageVisionWidget
  return never

viewWidget _ (LoadView 2) = do
  concentricCircleVisionWidget
  return never


viewWidget _ (IFrame url) = do
  let attrs = Map.fromList [("src",url), ("style","height:100%"), ("allow","microphone *")]
  elAttr "iframe" attrs $ return ()
  return never

zoneWidget :: (MonadWidget t m, Eq a)
  => Int -> a -> (Definition -> Maybe a) -> (a -> Definition) -> Event t [EnsembleResponse]
  -> (Dynamic t a -> W t m (Variable t a))
  -> W t m (Event t EnsembleRequest)
zoneWidget z defaultA f g ensResponses anEditorWidget = do
  iZones <- zones >>= sample . current
  let iDef = IntMap.findWithDefault (g defaultA) z iZones
  let iValue = maybe defaultA id $ f iDef
  let resetValue = g defaultA
  let deltas = fmapMaybe (lastEditOrResetInZone resetValue z) ensResponses
  let deltas' = fmapMaybe f deltas
  dynUpdates <- holdDyn iValue deltas'
  variableFromWidget <- anEditorWidget dynUpdates
  return $ (WriteZone z . g) <$> localEdits variableFromWidget

  -- a helper function

snippetOrExample:: Bool -> T.Text
snippetOrExample True = "example code-font"
snippetOrExample False = "snippet code-font"
