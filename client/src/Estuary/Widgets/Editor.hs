module Estuary.Widgets.Editor where

-- In this module, we define a type that represents the greater majority of widgets
-- in the Estuary project. Such Editor widgets have access to shared information about the
-- "Context" (user prefs, etc) and render engine (eg. audio and load levels) and are
-- able to propagate Hint-s upwards as necessary, alongside returning some value.
-- It also defines the Reflex-extending type Variable to cover a common case where we
-- need to keep track of whether a value changes because of remote editing or local editing.


import Reflex
import Reflex.Dom
import Control.Monad.Reader
import Data.Text
import Data.Tuple.All

import Estuary.Types.Context
import Estuary.Types.RenderInfo
import Estuary.Types.Hint
import Estuary.Types.TranslatableText
import Estuary.Types.Term


-- If we have a widget-producing action and we make it in the (Editor t m) monad, instead of in the more
-- general "reflex" monad (m) we will be able to do all the normal things we can do in the more general
-- monad (ie. "reflex things") plus the additional actions defined here:

type Editor t m = EventWriterT t [Hint] (ReaderT (ImmutableRenderContext,Dynamic t Context,Dynamic t RenderInfo) m)

-- We only need to use runEditor if we are embedding an Editor in a different kind of widget.
runEditor :: MonadWidget t m => ImmutableRenderContext -> Dynamic t Context -> Dynamic t RenderInfo -> Editor t m a -> m (a,Event t [Hint])
runEditor irCtx ctx ri e = runReaderT (runEventWriterT e) (irCtx,ctx,ri)


-- Get the immutable render context.
immutableRenderContext :: MonadWidget t m => Editor t m ImmutableRenderContext
immutableRenderContext = lift $ asks sel1


-- Get the dynamic context.
context :: MonadWidget t m => Editor t m (Dynamic t Context)
context = lift $ asks sel2


-- Get the dynamic information from the render engine.
renderInfo :: MonadWidget t m => Editor t m (Dynamic t RenderInfo)
renderInfo = lift $ asks sel3


-- Issue a single hint
hint :: MonadWidget t m => Event t Hint -> Editor t m ()
hint = tellEvent . fmap (:[])


-- Issue multiple simultaneous hints
hints :: MonadWidget t m => Event t [Hint] -> Editor t m ()
hints = tellEvent


-- Translate a term appropriately into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
term :: MonadWidget t m => Term -> Editor t m (Dynamic t Text)
term t = do
  c <- context
  l <- holdUniqDyn $ fmap language c
  return $ translate t <$> l


-- Translate a TranslatableText (eg. paragraph) into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
translatableText :: MonadWidget t m => TranslatableText -> Editor t m (Dynamic t Text)
translatableText t = do
  c <- context
  l <- holdUniqDyn $ fmap language c
  return $ translateText t <$> l


-- Translate a dynamic TranslatableText into dynamic text
-- Note that it doesn't build the text in the DOM - for that, combine with dynText
dynTranslatableText :: MonadWidget t m => Dynamic t TranslatableText -> Editor t m (Dynamic t Text)
dynTranslatableText t = do
  c <- context
  l <- holdUniqDyn $ fmap language c
  return $ translateText <$> t <*> l


-- The Variable type abstracts around a situation that is very common across the Estuary
-- project - wanting to represent values that might be changed either by local edits or
-- "other causes" (eg. network edits), where we need to keep track of both local edits,
-- and the value from all changes.

data Variable t a = Variable {
  currentValue :: Dynamic t a,
  localEdits :: Event t a
  }

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

returnVariable :: (Monad m, Reflex t, MonadSample t m, MonadHold t m) => Dynamic t a -> Event t a -> m (Variable t a)
returnVariable deltasDown editsUp = do
  i <- sample $ current deltasDown
  val <- holdDyn i $ leftmost [editsUp, updated deltasDown]
  return $ Variable val editsUp

-- the former reflexWidgetToEditor...
variableWidget :: MonadWidget t m => Dynamic t a -> (a -> Event t a -> m (Event t a)) -> m (Variable t a)
variableWidget delta widget = do
  i <- sample $ current delta
  x <- widget i $ updated delta
  returnVariable delta x

flattenDynamicVariable :: Reflex t => Dynamic t (Variable t a) -> Variable t a
flattenDynamicVariable x = Variable d e
  where
    d = join $ fmap currentValue x -- Dynamic (Dynamic t a)
    e = switchPromptlyDyn $ fmap localEdits x -- Dynamic (Event t a)
