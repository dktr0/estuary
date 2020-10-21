module Estuary.Widgets.W where

-- In this module, we define a monadic type (W t m) that represents the greater majority
-- of widgets in the Estuary project. Such widgets have access to various kinds of information
-- from different places, and propagate changes to settings, Hint-s, EnsembleEvent-s, and Request-s
-- upwards as necessary, alongside returning some value.

-- If we have a widget-producing action and we make it in the (W t m) monad, instead of in the more
-- general "reflex" monad (m) we will be able to do all the normal things we can do in the more general
-- monad (ie. "reflex things") plus the additional actions defined here:

import Data.Text
import Control.Monad.Reader
import Control.Monad.State
import Reflex
import Reflex.Dom

import Estuary.Types.Settings
import Estuary.Types.NetworkStatus
import Estuary.Types.EnsembleC
import Estuary.Types.RenderInfo
import Estuary.Types.Hint
import Estuary.Types.TranslatableText
import Estuary.Types.Term

-- 1. we need to be able to access/read these things when defining widgets...
type WidgetEnvironment t = (Dynamic t Settings, Dynamic t NetworkStatus, EnsembleC, Dynamic t RenderInfo)

-- 2. and we need to be able to collect all of the following effects in each frame of UI interaction...
type WidgetEffects t = (Event t (Settings -> Settings), Event t [Hint], Event t [EnsembleEvent], Event t [Request])

-- 3. and we'll need to build widgets in some monad m provided by reflex-dom,
-- with some reflex FRP timeline t, so we bring these three things together
-- using monad transformers, as the new monad (W t m):

type W t m = StateT (WidgetEffects t) (ReaderT (WidgetEnvironment t) m)

runW :: MonadWidget t m =>
  Dynamic t Settings ->
  Dynamic t NetworkStatus ->
  EnsembleC ->
  Dynamic t RenderInfo ->
  W t m a ->
  m (a,Event t (Settings -> Settings),Event t [Hint],Event t [EnsembleEvent],Event t [Request])
runW s ns ec ri x = do
  (a,(s',hs,eevs,rqs)) <- runReaderT (runStateT x (never,never,never,never)) (s,ns,ec,ri)
  return (a,s',hs,eevs,rqs)


-- now on the basis of the above, we can define functions for reading the environment,
-- setting/signalling settings changes, hints, EnsembleEvent-s, and Request-s, etc

-- read the current settings (including current language, theme, render settings, etc)
settings :: W t m (Dynamic t Settings)
settings = do
  (x,_,_,_) <- ask
  return x

-- read the current network status (including announcements, server and websocket status, etc)
networkStatus :: W t m (Dynamic t NetworkStatus)
networkStatus = do
  (_,x,_,_) <- ask
  return x

-- read the current ensemble (including file resources, zones, views, etc)
ensembleC :: W t m EnsembleC
ensembleC = do
  (_,_,x,_) <- ask
  return x

-- read the current render info (time and load information from the render process)
renderInfo :: W t m (Dynamic t RenderInfo)
renderInfo = do
  (_,_,_,x) <- ask
  return x

-- set/signal a change to client Settings
changeSettings :: Event t (Settings -> Settings) -> W t m ()
changeSettings x = do
  (s,hs,eevs,rqs) <- get
  put (mergeWith (.) [x,s],hs,eevs,rqs)

-- set/signal a Hint
hint :: Event t Hint -> W t m ()
hint x = do
  (s,hs,eevs,rqs) <- get
  put $ (s, mergeWith (++) [pure x,hs],eevs,rqs)

-- set/signal an EnsembleEvent
ensembleEvent :: Event t EnsembleEvent -> W t m ()
ensembleEvent x = do
  (s,hs,eevs,rqs) <- get
  put $ (s,hs,mergeWith (++) [pure x,eevs],rqs)

-- set/signal a Request
request :: Event t Request -> W t m ()
request x = do
  (s,hs,eevs,rqs) <- get
  put $ (s,hs,eevs,mergeWith (++) [pure x,rqs])


-- Translate a term appropriately into dynamic text
-- Note that it doesn't build the text in the DOM - for that, use term (below) instead
translateTerm :: Term -> W t m (Dynamic t Text)
translateTerm t = do
  l <- language <$> settings
  return $ translate t <$> l

term :: Term -> W t m ()
term t = translateTerm t >>= dynText

-- Translate a TranslatableText (eg. paragraph) into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with eg. dynText
translatableText :: TranslatableText -> W t m (Dynamic t Text)
translatableText t = do
  l <- language <$> settings
  return $ translateText t <$> l

-- Translate a dynamic TranslatableText into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with eg. dynText
dynTranslatableText :: Dynamic t TranslatableText -> W t m (Dynamic t Text)
dynTranslatableText t = do
  l <- language <$> settings
  return $ translateText <$> t <*> l
