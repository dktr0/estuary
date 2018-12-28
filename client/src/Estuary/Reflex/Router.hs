{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

module Estuary.Reflex.Router(
  router,
  getInitialState
) where

import Control.Monad.IO.Class

import Data.Maybe

import GHCJS.DOM.EventM(on, event)
import GHCJS.DOM.Types(ToJSString(..), FromJSString(..))

import GHCJS.DOM(currentWindow,)
import GHCJS.DOM.History
import qualified GHCJS.DOM.PopStateEvent as PopStateEvent
import qualified GHCJS.DOM.Window as Window(getHistory)
import qualified GHCJS.DOM.WindowEventHandlers as Window (popState)

import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Nullable

import Reflex
import Reflex.Dom

-- currentWindow :: IO (Maybe Window)
-- getHistory :: Window -> IO (Maybe History)
-- pushState :: (MonadIO m, ToJSString title, ToJSString url) =>
--     History -> JSVal -> title -> url -> m ()

router :: (MonadWidget t m, FromJSVal state, ToJSVal state) => state -> Event t state -> (state -> m (Event t state, a)) -> m (Dynamic t (Event t state, a))
router def inStatChangeEv renderPage = mdo
  let initialPage = renderPage def

  -- Triggered ambiently (back button or otherwise). If the state is null or can't
  -- be decoded, fall back into the initial state.
  popStateEv :: Event t state <- fmap (fromMaybe def) <$> getPopStateEv

  -- Triggered via a page widget (dynPage is the recursive value from further below).
  -- When a change is explicitly triggered, we notify the history via pushPageState.
  dynStateChangeEv :: Dynamic t (Event t state) <- mapDyn fst dynPage
  let triggeredStateChangeEv = leftmost [
          inStatChangeEv,
          switchPromptlyDyn dynStateChangeEv
        ]
  performEvent_ $ ffor triggeredStateChangeEv $ \state -> liftIO $ do
    pushPageState state ""

  -- The router state is changed when either the browser buttons are pressed or
  -- a child page triggers a change.
  let stateChangeEv = leftmost [popStateEv, triggeredStateChangeEv]

  dynPage :: Dynamic t (Event t state, a) <- widgetHold initialPage (renderPage <$> stateChangeEv)

  return dynPage

getInitialState :: (FromJSVal state) => state -> IO (state)
getInitialState def =
  maybeIO def currentWindow $ \window ->
    maybeIO def (Just <$> Window.getHistory window) $ \history -> do
      maybeIO def (pFromJSVal <$> pToJSVal <$> getState history) $ \state ->
        maybeIO def (liftIO $ fromJSVal state) return

pushPageState :: (ToJSVal state, ToJSString url) => state -> url -> IO ()
pushPageState state url = do
  maybeIO () currentWindow $ \window ->
    maybeIO () (Just <$> Window.getHistory window) $ \history -> do
      jsState <- liftIO $ toJSVal state
      -- Mozilla reccomends to pass "" as title to keep things future proof
      pushState history jsState "" (Just url)

getPopStateEv :: (MonadWidget t m, FromJSVal state) => m (Event t (Maybe state))
getPopStateEv = do
  mWindow <- liftIO $ currentWindow
  case mWindow of
    Nothing -> return never
    Just window ->
      wrapDomEvent window (\e -> on e Window.popState) $ do
        -- in (EventM t PopState) which is (ReaderT PopState IO)
        eventData <- event -- ask
        nullableJsState <- liftIO $ pFromJSVal <$> pToJSVal <$> PopStateEvent.getState eventData
        case nullableJsState of
          Nothing -> return Nothing
          Just jsState -> liftIO $ fromJSVal jsState


maybeIO :: b -> IO (Maybe a) -> (a -> IO b) -> IO b
maybeIO def computeA computeBFromA = do
  val <- computeA
  case val of
    Nothing -> return def
    Just a -> computeBFromA a
