module Estuary.Widgets.EstuaryWidget where

import Reflex
import Reflex.Dom

import Estuary.Types.Hint
import Estuary.Types.Context
import Estuary.RenderInfo

data EstuaryWidget t m a b = EstuaryWidget {
  widget :: Dynamic t Context -> Dynamic t RenderInfo -> a -> Event t a -> m (b, Event t a, Event t Hint)
  }

runEstuaryWidget :: MonadWidget t m => EstuaryWidget t m a b
  -> Dynamic t Context -> Dynamic t RenderInfo -> a -> Event t a
  -> m (Dynamic t a, Event t a, Event t Hint)
runEstuaryWidget e ctx ri i d = do
  (b,edits,hints) <- (widget e) ctx ri i d
  v <- holdDyn i $ leftmost [edits,d]
  return (v,edits,hints)

instance MonadWidget t m => Functor (EstuaryWidget t m a) where
  fmap f x = EstuaryWidget $ \ctx ri i d -> do
    (b,edits,hints) <- (widget x) ctx ri i d
    return (f b,edits,hints)

instance MonadWidget t m => Applicative (EstuaryWidget t m a) where
  pure x = EstuaryWidget $ \_ _ _ _ -> return (x, never, never)
  f <*> a = EstuaryWidget $ \ctx ri i d -> do
    (f',edit1,hint1) <- (widget f) ctx ri i d
    (a',edit2,hint2) <- (widget a) ctx ri i d
    let combinedEdits = leftmost [edit2,edit1]
    let combinedHints = leftmost [hint1,hint2]
    return (f' a',combinedEdits,combinedHints)

instance MonadWidget t m => Monad (EstuaryWidget t m a) where
  x >>= f = EstuaryWidget $ \ctx ri v deltaV -> do
    (a,edit1,hint1) <- (widget x) ctx ri v deltaV
    (b,edit2,hint2) <- (widget $ f a) ctx ri v deltaV
    let combinedEdits = leftmost [edit2,edit1]
    let combinedHints = leftmost [hint1,hint2]
    return (b,combinedEdits,combinedHints)

context :: MonadWidget t m => EstuaryWidget t m a (Dynamic t Context)
context = EstuaryWidget $ \ctx _ _ _ -> return (ctx, never, never)

renderInfo :: MonadWidget t m => EstuaryWidget t m a (Dynamic t RenderInfo)
renderInfo = EstuaryWidget $ \_ ri _ _ -> return (ri, never, never)

initialValue :: MonadWidget t m => EstuaryWidget t m a a
initialValue = EstuaryWidget $ \_ _ a _ -> return (a, never, never)

delta :: MonadWidget t m => EstuaryWidget t m a (Event t a)
delta = EstuaryWidget $ \_ _ _ d -> return (d, never, never)

edit :: MonadWidget t m => Event t a -> EstuaryWidget t m a ()
edit x = EstuaryWidget $ \_ _ _ _ -> return ((),x,never)

hint :: MonadWidget t m => Event t Hint -> EstuaryWidget t m a ()
hint x = EstuaryWidget $ \_ _ _ _ -> return ((),never,x)

reflex :: MonadWidget t m => m b -> EstuaryWidget t m a b
reflex m = EstuaryWidget $ \_ _ _ _ -> do
  b <- m
  return (b,never,never)

reflex' :: MonadWidget t m => (a -> Event t a -> m b) -> EstuaryWidget t m a b
reflex' m = EstuaryWidget $ \_ _ i d -> do
  b <- m i d
  return (b,never,never)

embed :: MonadWidget t m => (c -> a) -> EstuaryWidget t m a b -> EstuaryWidget t m c (b,Event t a)
embed f w = EstuaryWidget $ \ctx ri initialC deltaC -> do
  let initialA = f initialC
  let deltaA = fmap f deltaC
  (b,editsA,hints) <- (widget w) ctx ri initialA deltaA
  return ((b,editsA),never,hints)

embed_ :: MonadWidget t m => (c -> a) -> EstuaryWidget t m a b -> EstuaryWidget t m c (Event t a)
embed_ f w = EstuaryWidget $ \ctx ri initialC deltaC -> do
  let initialA = f initialC
  let deltaA = fmap f deltaC
  (_,editsA,hints) <- (widget w) ctx ri initialA deltaA
  return (editsA,never,hints)

textArea' :: MonadWidget t m => String -> Event t String -> m (Event t String)
textArea' _ _ = return never -- ...this is just to compile-check the examples below...

example :: MonadWidget t m => EstuaryWidget t m String ()
example = do
  i <- initialValue
  d <- delta
  reflex (textArea' i d) >>= edit

example2 :: MonadWidget t m => EstuaryWidget t m String ()
example2 = reflex' textArea' >>= edit

example3 :: MonadWidget t m => EstuaryWidget t m (String,String) ()
example3 = do
  reflex $ text "editor1"
  x <- embed_ fst example2 -- x :: Event t String
  reflex $ text "editor2"
  y <- embed_ snd example2 -- x :: Event t String
  i <- initialValue
  x' <- reflex $ holdDyn (fst i) x
  y' <- reflex $ holdDyn (snd i) y
  xy <- reflex $ combineDyn (\a b -> (a,b)) x' y'
  edit $ updated xy

-- working here: example3 is okay... but it would be really nice to not have to
-- put all that boilerplate stuff in the second half of the definition re-combining
-- the two components of the "value of interest"...
