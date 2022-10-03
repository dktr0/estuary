{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo #-}

module Estuary.Widgets.W where

-- In this module, we define a type that represents the greater majority of widgets
-- in the Estuary project. Widgets built in the W monad (technically: the (W t) monad)
-- have access to shared information about the "Context" (user prefs, etc) and render
-- engine (eg. audio and load levels) and are able to propagate Hint-s upwards as necessary,
-- alongside returning some value. It also defines the Reflex-extending type Variable to
-- cover a common case where we need to keep track of whether a value changes because of
-- remote editing or local editing.

import Reflex
import Reflex.Dom
import Control.Monad.Reader
import Data.Text as T
import Data.IORef
import Data.Time
import Data.Map as Map
import Text.Read (readMaybe)
import Data.IntMap
import Data.Maybe

import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Types.Language
import Estuary.Types.Context
import qualified Estuary.Render.R as R
import Estuary.Render.DynamicsMode
import Estuary.Types.RenderInfo
import Estuary.Types.Hint
import Estuary.Types.TranslatableText
import Estuary.Types.Term
import Estuary.Resources
import qualified Estuary.Client.Settings as Settings


-- If we have widget-producing actions and we make them in the (W t m) monad
-- we will be able to do all the normal things we can do in the more general
-- monad (ie. "reflex things") plus the additional actions defined here!

type W t m = EventWriterT t [Hint] (ReaderT (WidgetEnvironment t) m)

data WidgetEnvironment t = WidgetEnvironment {
  _renderEnvironment :: R.RenderEnvironment,
  _context :: Dynamic t Context,
  _renderInfo :: Dynamic t RenderInfo,
  _resourceMaps :: Dynamic t ResourceMaps,
  _settings :: Dynamic t Settings.Settings
  }

-- runW is used to embed a W widget in a different kind of widget. (This should mostly
-- only be necessary at the very top of Estuary's widget hierarchy.)
runW :: (Reflex t, Monad m) => WidgetEnvironment t -> W t m a -> m (a,Event t [Hint])
runW wEnv e = runReaderT (runEventWriterT e) wEnv


-- Get the entire environment provided to the widget. Note that it should normally
-- be preferable to use the functions below to directly get specific elements from
-- the environment. This definition is provided "just in case" it should be useful...
widgetEnvironment :: Monad m => W t m (WidgetEnvironment t)
widgetEnvironment = lift ask


-- Get the immutable render context.
renderEnvironment :: Monad m => W t m R.RenderEnvironment
renderEnvironment = lift $ asks _renderEnvironment

-- Get the dynamic context.
context :: Monad m => W t m (Dynamic t Context)
context = lift $ asks _context

-- Get the dynamic information from the render engine.
renderInfo :: Monad m => W t m (Dynamic t RenderInfo)
renderInfo = lift $ asks _renderInfo

-- Get a dynamically-updated map of the current maps of "fixed" resources (audiofiles, images, videos)
resourceMaps :: Monad m => W t m (Dynamic t ResourceMaps)
resourceMaps = lift $ asks _resourceMaps

-- Get and change the Dynamic Settings record
settings :: Monad m => W t m (Dynamic t Settings.Settings)
settings = lift $ asks Estuary.Widgets.W._settings

-- Get and set specific client settings...

setSetting :: (Reflex t, Monad m) => Event t (Settings.Settings -> Settings.Settings) -> W t m ()
setSetting x = hint $ fmap ChangeSettings x

askSetting :: (Reflex t, MonadFix m, MonadHold t m, Eq a) => (Settings.Settings -> a) -> W t m (Dynamic t a)
askSetting f = settings >>= holdUniqDyn . fmap f

language :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Language)
language = askSetting Settings.language

setLanguage :: (Reflex t, Monad m) => Event t Language -> W t m ()
setLanguage x = setSetting $ fmap (\b s -> s { Settings.language = b } ) x

theme :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Text)
theme = askSetting Settings.theme

setTheme :: (Reflex t, Monad m) => Event t Text -> W t m ()
setTheme x = setSetting $ fmap (\b s -> s { Settings.theme = b } ) x

terminalVisible :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Bool)
terminalVisible = askSetting Settings.terminalVisible

setTerminalVisible :: (Reflex t, Monad m) => Event t Bool -> W t m ()
setTerminalVisible x = setSetting $ fmap (\b s -> s { Settings.terminalVisible = b } ) x

sideBarVisible :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Bool)
sideBarVisible = askSetting Settings.sideBarVisible

setSideBarVisible :: (Reflex t, Monad m) => Event t Bool -> W t m ()
setSideBarVisible x = setSetting $ fmap (\b s -> s { Settings.sideBarVisible = b } ) x

statsVisible :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Bool)
statsVisible = askSetting Settings.statsVisible

setStatsVisible :: (Reflex t, Monad m) => Event t Bool -> W t m ()
setStatsVisible x = setSetting $ fmap (\b s -> s { Settings.statsVisible = b } ) x

headerVisible :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Bool)
headerVisible = askSetting Settings.headerVisible

setHeaderVisible :: (Reflex t, Monad m) => Event t Bool -> W t m ()
setHeaderVisible x = setSetting $ fmap (\b s -> s { Settings.headerVisible = b } ) x

canvasOn :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Bool)
canvasOn = askSetting Settings.canvasOn

setCanvasOn :: (Reflex t, Monad m) => Event t Bool -> W t m ()
setCanvasOn x = setSetting $ fmap (\b s -> s { Settings.canvasOn = b } ) x

resolution :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Punctual.Resolution)
resolution = askSetting Settings.resolution

setResolution :: (Reflex t, Monad m) => Event t Punctual.Resolution -> W t m ()
setResolution x = setSetting $ fmap (\b s -> s { Settings.resolution = b } ) x

brightness :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Double)
brightness = askSetting Settings.brightness

setBrightness :: (Reflex t, Monad m) => Event t Double -> W t m ()
setBrightness x = setSetting $ fmap (\b s -> s { Settings.brightness = b } ) x

fpsLimit :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t (Maybe NominalDiffTime))
fpsLimit = askSetting Settings.fpsLimit

setFpsLimit :: (Reflex t, Monad m) => Event t (Maybe NominalDiffTime) -> W t m ()
setFpsLimit x = setSetting $ fmap (\b s -> s { Settings.fpsLimit = b } ) x

cineCer0ZIndex :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Int)
cineCer0ZIndex = askSetting Settings.cineCer0ZIndex

setCineCer0ZIndex :: (Reflex t, Monad m) => Event t Int -> W t m ()
setCineCer0ZIndex x = setSetting $ fmap (\b s -> s { Settings.cineCer0ZIndex = b } ) x

punctualZIndex :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Int)
punctualZIndex = askSetting Settings.punctualZIndex

setPunctualZIndex :: (Reflex t, Monad m) => Event t Int -> W t m ()
setPunctualZIndex x = setSetting $ fmap (\b s -> s { Settings.punctualZIndex = b } ) x

improvizZIndex :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Int)
improvizZIndex = askSetting Settings.improvizZIndex

setImprovizZIndex :: (Reflex t, Monad m) => Event t Int -> W t m ()
setImprovizZIndex x = setSetting $ fmap (\b s -> s { Settings.improvizZIndex = b } ) x

hydraZIndex :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Int)
hydraZIndex = askSetting Settings.hydraZIndex

setHydraZIndex :: (Reflex t, Monad m) => Event t Int -> W t m ()
setHydraZIndex x = setSetting $ fmap (\b s -> s { Settings.hydraZIndex = b } ) x

webDirtOn :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Bool)
webDirtOn = askSetting Settings.webDirtOn

setWebDirtOn :: (Reflex t, Monad m) => Event t Bool -> W t m ()
setWebDirtOn x = setSetting $ fmap (\b s -> s { Settings.webDirtOn = b } ) x

unsafeModeOn :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Bool)
unsafeModeOn = askSetting Settings.unsafeModeOn

setUnsafeModeOn :: (Reflex t, Monad m) => Event t Bool -> W t m ()
setUnsafeModeOn x = setSetting $ fmap (\b s -> s { Settings.unsafeModeOn = b } ) x

superDirtOn :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Bool)
superDirtOn = askSetting Settings.superDirtOn

setSuperDirtOn :: (Reflex t, Monad m) => Event t Bool -> W t m ()
setSuperDirtOn x = setSetting $ fmap (\b s -> s { Settings.superDirtOn = b } ) x

dynamicsMode :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t DynamicsMode)
dynamicsMode = askSetting Settings.dynamicsMode

setDynamicsMode :: (Reflex t, Monad m) => Event t DynamicsMode -> W t m ()
setDynamicsMode x = setSetting $ fmap (\b s -> s { Settings.dynamicsMode = b } ) x

globalAudioDelay :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t Double)
globalAudioDelay = askSetting Settings.globalAudioDelay

setGlobalAudioDelay :: (Reflex t, Monad m) => Event t Double -> W t m ()
setGlobalAudioDelay x = setSetting $ fmap (\b s -> s { Settings.globalAudioDelay = b } ) x

punctualAudioInputMode :: (Reflex t, MonadFix m, MonadHold t m) => W t m (Dynamic t PunctualAudioInputMode)
punctualAudioInputMode = askSetting Settings.punctualAudioInputMode

setPunctualAudioInputMode :: (Reflex t, Monad m) => Event t PunctualAudioInputMode -> W t m ()
setPunctualAudioInputMode x = setSetting $ fmap (\b s -> s { Settings.punctualAudioInputMode = b } ) x


-- some Bool Settings can be toggled

toggleHeaderVisible :: (Reflex t, MonadFix m, MonadHold t m) => Event t a -> W t m ()
toggleHeaderVisible x = do
  y <- current <$> headerVisible
  setHeaderVisible $ attachWith (\a _ -> not a) y x

toggleSideBarVisible :: (Reflex t, MonadFix m, MonadHold t m) => Event t a -> W t m ()
toggleSideBarVisible x = do
  y <- current <$> sideBarVisible
  setSideBarVisible $ attachWith (\a _ -> not a) y x

toggleStatsVisible :: (Reflex t, MonadFix m, MonadHold t m) => Event t a -> W t m ()
toggleStatsVisible x = do
  y <- current <$> statsVisible
  setStatsVisible $ attachWith (\a _ -> not a) y x

toggleTerminalVisible :: (Reflex t, MonadFix m, MonadHold t m) => Event t a -> W t m ()
toggleTerminalVisible x = do
  y <- current <$> terminalVisible
  setTerminalVisible $ attachWith (\a _ -> not a) y x


-- A basic checkbox widget that is updated from elsewhere (eg. collaborative editing)
-- and which issues events when it is changed by local input only.

checkboxW :: MonadWidget t m => Dynamic t Bool -> m (Event t Bool)
checkboxW x = do
  i <- sample $ current x
  let xEvents = updated x
  _checkbox_change <$> checkbox i (def & checkboxConfig_setValue .~ xEvents)


-- A basic dropdown menu that is updated from elsewhere (eg. collaborative editing)
-- and which issues events when it is changed by local input only.
-- *** Note: currently introduces some kind of cyclic dataflow problem when used with hints/W monad - still DEBUGGING

dropdownW :: (MonadWidget t m, Ord k) => Map k Text -> Dynamic t k -> m (Event t k)
dropdownW m x = divClass "config-entry display-inline-block primary-color ui-font" $ do
  let m' = constDyn m
  i <- sample $ current x
  let xEvents = updated x
  d <- dropdown i m' (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" ) & dropdownConfig_setValue .~ xEvents)
  return $ _dropdown_change d
  -- return $ gate (current $ constDyn False) y


-- A text input area that is updated from elsewhere (eg. collaborative editing),
-- issues events in response to valid local input only

intTextInputW :: MonadWidget t m => Dynamic t Int -> m (Event t Int)
intTextInputW x = do
  i <- sample $ current x
  let xEvents = updated $ fmap (T.pack . show) x
  w <- textInput $ def & textInputConfig_inputType .~ "number"
                       & textInputConfig_initialValue .~ T.pack (show i)
                       & attributes .~ constDyn ("class" =: "ui-inputMenus primary-color primary-borders ui-font")
                       & textInputConfig_setValue .~ xEvents
  return $ fmapMaybe (readMaybe . T.unpack) $ _textInput_input w


textInputW :: MonadWidget t m => Dynamic t (Map Text Text) -> Dynamic t Text -> m (Event t Text)
textInputW inputHint x = do
  i <- sample $ current x
  let xEvents = updated x
  w <- textInput $ def & textInputConfig_initialValue .~ i
                       & attributes .~ (constDyn $ "class" =: "ui-inputMenus primary-color primary-borders ui-font") <> inputHint
                       & textInputConfig_setValue .~ xEvents
  return $ _textInput_input w



-- Issue a single hint
hint :: (Reflex t, Monad m) => Event t Hint -> W t m ()
hint = tellEvent . fmap pure

-- Issue multiple simultaneous hints
hints :: (Reflex t, Monad m) => Event t [Hint] -> W t m ()
hints = tellEvent


-- Translate a term appropriately into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
term :: (Reflex t, Monad m, MonadHold t m, MonadFix m) => Term -> W t m (Dynamic t Text)
term t = do
  l <- language
  return $ translate t <$> l


-- Translate a TranslatableText (eg. paragraph) into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
translatableText :: (Reflex t, Monad m, MonadHold t m, MonadFix m) => TranslatableText -> W t m (Dynamic t Text)
translatableText t = do
  l <- language
  return $ translateText t <$> l


-- Translate a dynamic TranslatableText into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
dynTranslatableText :: (Reflex t, Monad m, MonadHold t m, MonadFix m) => Dynamic t TranslatableText -> W t m (Dynamic t Text)
dynTranslatableText t = do
  l <- language
  return $ translateText <$> t <*> l


-- The Variable type abstracts around a situation that is very common across the Estuary
-- project - wanting to represent values that might be changed either by local edits or
-- "other causes" (eg. network edits), where we need to keep track of both local edits,
-- and the value from all changes (local )

data Variable t a = Variable {
  currentValue :: Dynamic t a,
  localEdits :: Event t a
  }

-- The 'variable' function provides a commonly used interface for constructing values :: Variable t a
-- Given an initial value + only changes from elsewhere (Dynamic t a) and an Event representing only local changes
-- it combines them to produce a Variable t a, in such a way that the Variable's currentValue reflects both
-- remote and local changes.

variable :: (Monad m, Reflex t, MonadSample t m, MonadHold t m) => Dynamic t a -> Event t a -> m (Variable t a)
variable deltasDown editsUp = do
  i <- sample $ current deltasDown
  val <- holdDyn i $ leftmost [editsUp, updated deltasDown]
  return $ Variable val editsUp

returnVariable :: (Monad m, Reflex t, MonadSample t m, MonadHold t m) => Dynamic t a -> Event t a -> m (Variable t a)
returnVariable = variable -- deprecated synonym for variable (above)

initialValue :: (Monad m, Reflex t, MonadSample t m) => Variable t a -> m a
initialValue = sample . current . currentValue

instance Reflex t => Functor (Variable t) where
  fmap f (Variable d e) = Variable (fmap f d) (fmap f e)

instance Reflex t => Applicative (Variable t) where
  pure x = Variable (constDyn x) never
  (Variable fDyn fEdit) <*> (Variable xDyn xEdit) = Variable d e
    where
     d = fDyn <*> xDyn
     e = tagPromptlyDyn d $ leftmost [() <$ fEdit,() <$ xEdit]

instance Reflex t => Monad (Variable t) where
  (Variable aDyn aEvent) >>= f = Variable dynResult evResult
    where
      dynVar = fmap f aDyn -- :: Dynamic t (Variable t b)
      dynResult = join $ fmap currentValue dynVar -- :: Dynamic t b
      fEvent = switchPromptlyDyn $ fmap localEdits dynVar -- Event t b
      evResult = tagPromptlyDyn dynResult $ leftmost [() <$ aEvent, () <$ fEvent]

instance (Reflex t, Semigroup a) => Semigroup (Variable t a) where
  (Variable d1 e1) <> (Variable d2 e2) = Variable d e
    where
      d = d1 <> d2
      e = mergeWith (<>) [attachPromptlyDynWith (<>) d1 e2,attachPromptlyDynWith (flip (<>)) d2 e1]

instance (Reflex t, Monoid a) => Monoid (Variable t a) where
  mempty = Variable (constDyn mempty) never

variableWidget :: (Reflex t, Monad m, MonadSample t m, MonadHold t m) => Dynamic t a -> (a -> Event t a -> m (Event t a)) -> m (Variable t a)
variableWidget delta widget = do
  i <- sample $ current delta
  x <- widget i $ updated delta
  variable delta x

flattenDynamicVariable :: Reflex t => Dynamic t (Variable t a) -> Variable t a
flattenDynamicVariable x = Variable d e
  where
    d = join $ fmap currentValue x -- Dynamic (Dynamic t a)
    e = switchPromptlyDyn $ fmap localEdits x -- Dynamic (Event t a)

distributeIntMapOverVariable :: Reflex t => IntMap (Variable t a) -> Variable t (IntMap a)
distributeIntMapOverVariable iMap = Variable curVal locEdits
  where
    curVal = distributeIntMapOverDynPure $ fmap currentValue iMap
    locEdits = mergeInt $ fmap localEdits iMap  -- IntMap (Event t a)


-- we need the difference between a and b here if we are to have widgets that give back signals of different types
-- than the types of the values the "create" the widgets, such as a "delete me" signal, for example.
-- soon we expect to rework this so that builder function takes an Int argument representing that item's position in map
-- and later we expect to rework this to avoid rebuilds whenever possible (eg. when values change but keys don't)

widgetMap :: (Show a, MonadWidget t m) => Dynamic t (IntMap a) -> (Dynamic t a -> m b) -> m (Dynamic t (IntMap b))
widgetMap delta buildF = do
  iMapA <- sample $ current delta
  let f m = mapM (buildF . constDyn) m -- f :: IntMap a -> m (IntMap b)
  let iWidget = f iMapA
  let rebuilds = fmap f $ updated delta
  widgetHold iWidget rebuilds

-- | widgetMapEvent is a variant of widgetMap, specialized for Event
widgetMapEvent :: (Show a, MonadWidget t m) => Dynamic t (IntMap a) -> (Dynamic t a -> m (Event t a)) -> m (Event t (IntMap a))
widgetMapEvent delta buildF = mdo
  x <- widgetMap delta buildF
  let evPartialMap = switchDyn $ fmap mergeIntMap x -- Event t (IntMap a), representing change to specific row(s) only
  let evFullMap = attachWith (flip Data.IntMap.union) (current localValue) evPartialMap
  iLocalValue <- sample $ current delta
  localValue <- holdDyn iLocalValue $ leftmost [updated delta,evFullMap]
  pure evFullMap

-- | widgetMapEventWithAdd is another variant, specialized for Event, with a channel to add items to the end of the map
widgetMapEventWithAdd :: (Show a, MonadWidget t m) => Dynamic t (IntMap a) -> Event t a -> (Dynamic t a -> m (Event t a)) -> m (Event t (IntMap a))
widgetMapEventWithAdd delta addEv buildF = mdo
  -- insert add events into dynamic stream of deltas
  iDelta <- sample $ current delta
  let addEv' = attachWith (\m a -> Data.IntMap.insert (Data.IntMap.size m) a m) (current displayedValue) addEv
  let evIntegratedDelta = leftmost [updated delta,addEv']
  dynIntegratedDelta <- holdDyn iDelta evIntegratedDelta
  editsBelow <- widgetMapEvent dynIntegratedDelta buildF
  displayedValue <- holdDyn iDelta $ leftmost [evIntegratedDelta, editsBelow]
  pure $ leftmost [editsBelow,addEv']

-- below this line are just demos of widgetMap etc above (code below this line will be deleted before too long)
--
-- widgetMapEventWithAddDelete :: (Show a, MonadWidget t m) => Dynamic t (IntMap a) -> Event t a -> (Dynamic t a -> m (Event t (Maybe a))) -> m (Event t (IntMap a))
-- widgetMapEventWithAddDelete delta addEv buildF = mdo
--     iDelta <- sample $ current delta
--     let addEv' = attachWith (\m a -> Data.IntMap.insert (Data.IntMap.size m) a m) (current displayedValue) addEv -- Event (IntMap a)
--     let evIntegratedDelta = leftmost [updated delta,addEv']  -- Event (IntMap a)
--     dynIntegratedDelta <- holdDyn iDelta evIntegratedDelta -- Dyn (IntMap a)
--     editsBelow <- widgetMapEvent' dynIntegratedDelta buildF
--     displayedValue <- holdDyn iDelta $ leftmost [evIntegratedDelta,  editsBelow]
--     pure $ leftmost [editsBelow,addEv']
--
-- -- let filterIWidgetNothing = liftM (Data.IntMap.filter isJust) iWidget -- m (IntMap (Maybe b)
--
-- widgetMap' :: (Show a, MonadWidget t m) => Dynamic t (IntMap a) -> (Dynamic t a -> m  b) -> m (Dynamic t (IntMap b))
-- widgetMap' delta buildF = do
--   iMapA <- sample $ current delta -- []
--   let f m = mapM (buildF . constDyn) m -- f :: IntMap a -> m (IntMap (Maybe b))
--   -- let iWidget = liftM localChangeOrDelete $ f iMapA -- m (IntMap (Maybe b))
--   let iWidget = f iMapA -- m (IntMap (Maybe b))
--   let rebuilds = fmap f $ updated delta -- Event t (m (Intmap (Maybe b))
--   -- let rebuilds' = fmap (liftM localChangeOrDelete) rebuilds
--   w <- widgetHold iWidget rebuilds -- m a -> Event t (m a) -> m (Dynamic t a)
--   return $ fmap (Data.IntMap.map fromJust . Data.IntMap.filter isJust) w
-- --
-- widgetMapEvent' :: (Show a, MonadWidget t m) => Dynamic t (IntMap a) -> (Dynamic t a -> m (Event t a)) -> m (Event t (IntMap a))
-- widgetMapEvent' delta buildF = mdo
--   x <- widgetMap' delta buildF -- Dynamic t (IntMap (Event t (Maybe a)))
--   let evPartialMap = switchDyn $ fmap mergeIntMap x -- Event t (IntMap (Maybe a)), representing change to specific row(s) only
--   -- let evPartialMap' = fmap (Data.IntMap.map fromJust . Data.IntMap.filter isJust) evPartialMap -- Event t (IntMap a)
--   let evFullMap = attachWith (flip Data.IntMap.union) (current localValue) evPartialMap ---- Event t (IntMap a)
--   iLocalValue <- sample $ current delta
--   localValue <- holdDyn iLocalValue $ leftmost [updated delta, evFullMap]
--   pure evFullMap

type Test = IntMap Text

testMap :: MonadWidget t m => Dynamic t Test -> m (Variable t Test)
testMap delta = do
  addButton <- traceEvent "AddButton" <$> button "+"
  deleteButton <- button "-"
  mapEv <- widgetMapEventWithAdd delta ("newtext" <$ addButton) testRow
  variable delta mapEv

testRow :: MonadWidget t m => Dynamic t Text -> m (Event t Text)
testRow delta = el "div" $ (textInputW (constDyn $ "placeholder" =: "") delta)

testRowMaybe :: MonadWidget t m => Dynamic t Text -> m (Event t (Maybe Text))
testRowMaybe delta = do
  deleteButton <- button "-"
  row <- el "div" $ (textInputW (constDyn $ "placeholder" =: "") delta) -- Event t Text
  let rowMaybe = fmap Just row -- Event t (Maybe Text)
  return $ leftmost [rowMaybe, Nothing <$ deleteButton]


testRowMaybe' :: MonadWidget t m => Dynamic t Text -> m (Event t Text)
testRowMaybe' delta = do
  deleteButton <- button "-"
  row <- el "div" $ (textInputW (constDyn $ "placeholder" =: "") delta) -- Event t Text
  return $ leftmost [row, "delete" <$ deleteButton]

-- Dynamic t a -> m (Event t (Maybe a))
-- Event t (Maybe a) where
--  Nothing = delete this row
--  Just x = local edit to this row

widgetMapDemo :: IO ()
widgetMapDemo = mainWidget $ do
  let i = Data.IntMap.singleton 0 "text zero"
  delta <- holdDyn i never
  x <- el "div" $ testMap delta -- :: Variable t Test

  -- display localEdits issued from widget
  el "div" $ do
    text "localEdits: "
    y <- holdDyn i $ localEdits x -- :: Dynamic t Test
    dynText $ fmap (T.pack . show) y

  -- display currentValue issued from widget
  el "div" $ do
    text "currentValue: "
    dynText $ fmap (T.pack . show) $ currentValue x
