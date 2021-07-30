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
import Data.Text

import Estuary.Types.Context
import Estuary.Types.RenderInfo
import Estuary.Types.Hint
import Estuary.Types.TranslatableText
import Estuary.Types.Term


-- If we have widget-producing actions and we make them in the (W t m) monad
-- we will be able to do all the normal things we can do in the more general
-- monad (ie. "reflex things") plus the additional actions defined here!

type W t m = EventWriterT t [Hint] (ReaderT (WidgetEnvironment t) m)

data WidgetEnvironment t = WidgetEnvironment {
  _immutableRenderContext :: ImmutableRenderContext,
  _context :: Dynamic t Context,
  _renderInfo :: Dynamic t RenderInfo
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
immutableRenderContext :: Monad m => W t m ImmutableRenderContext
immutableRenderContext = lift $ asks _immutableRenderContext


-- Get the dynamic context.
context :: Monad m => W t m (Dynamic t Context)
context = lift $ asks _context


-- Get the dynamic information from the render engine.
renderInfo :: Monad m => W t m (Dynamic t RenderInfo)
renderInfo = lift $ asks _renderInfo


-- Issue a single hint
hint :: (Reflex t, Monad m) => Event t Hint -> W t m ()
hint = tellEvent . fmap (:[])


-- Issue multiple simultaneous hints
hints :: (Reflex t, Monad m) => Event t [Hint] -> W t m ()
hints = tellEvent


-- Translate a term appropriately into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
term :: (Reflex t, Monad m, MonadHold t m, MonadFix m) => Term -> W t m (Dynamic t Text)
term t = do
  c <- context
  l <- holdUniqDyn $ fmap language c
  return $ translate t <$> l


-- Translate a TranslatableText (eg. paragraph) into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
translatableText :: (Reflex t, Monad m, MonadHold t m, MonadFix m) => TranslatableText -> W t m (Dynamic t Text)
translatableText t = do
  c <- context
  l <- holdUniqDyn $ fmap language c
  return $ translateText t <$> l


-- Translate a dynamic TranslatableText into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
dynTranslatableText :: (Reflex t, Monad m, MonadHold t m, MonadFix m) => Dynamic t TranslatableText -> W t m (Dynamic t Text)
dynTranslatableText t = do
  c <- context
  l <- holdUniqDyn $ fmap language c
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
