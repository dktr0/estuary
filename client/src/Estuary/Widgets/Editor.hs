module Estuary.Widgets.Editor where

-- In this module, we define a type that represents the greater majority of widgets
-- in the Estuary project. Such Editor widgets have access to shared information about the
-- "Context" (user prefs, etc) and render engine (eg. audio and load levels) and are
-- able to propagate Hint-s upwards as necessary, alongside returning some value.

-- If we have a widget-producing action and we make it in the (Editor t m) monad, instead of in the more
-- general "reflex" monad (m) we will be able to do all the normal things we can do in the more general
-- monad (ie. "reflex things") plus the additional actions defined here:

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
