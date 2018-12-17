module Estuary.Widgets.EstuaryWidget where

import Reflex
import Reflex.Dom

import Estuary.Types.Context
import Estuary.RenderInfo
import Estuary.Types.Hint

data EstuaryWidget t m a b = EstuaryWidget {
  widget :: Dynamic t Context -> Dynamic t RenderInfo -> Dynamic t a -> m (b, Dynamic t a, Event t Hint)
  } -- note: dynamic t a argument represents initial value + deltas, Dynamic t a result represents initial value + edits

runEstuaryWidget :: MonadWidget t m => EstuaryWidget t m a b
  -> Dynamic t Context -> Dynamic t RenderInfo -> Dynamic t a
  -> m (Dynamic t a, Dynamic t a, Event t Hint) -- (current value [deltas+edits],edits only, hints)
runEstuaryWidget e ctx ri d = do
  (_,edits,hints) <- (widget e) ctx ri d
  i <- (sample . current) d
  v <- holdDyn i $ leftmost [updated edits,updated d]
  return (v,edits,hints)

instance MonadWidget t m => Functor (EstuaryWidget t m a) where
  fmap f x = EstuaryWidget $ \ctx ri d -> do
    (a,edits,hints) <- (widget x) ctx ri d
    return (f a,edits,hints)

instance MonadWidget t m => Applicative (EstuaryWidget t m a) where
  pure x = EstuaryWidget $ \_ _ d -> do
    i <- (sample . current) d
    return (x, constDyn i, never)
  f <*> a = EstuaryWidget $ \ctx ri d -> do
    (f',edit1,hint1) <- (widget f) ctx ri d
    (a',edit2,hint2) <- (widget a) ctx ri d
    i <- (sample . current) d
    let combinedEdits = leftmost [updated edit2,updated edit1,updated d]
    combinedEdits' <- holdDyn i combinedEdits
    let combinedHints = leftmost [hint1,hint2]
    return (f' a',combinedEdits',combinedHints)

instance MonadWidget t m => Monad (EstuaryWidget t m a) where
  x >>= f = EstuaryWidget $ \ctx ri d -> do
    (a,edit1,hint1) <- (widget x) ctx ri d
    (b,edit2,hint2) <- (widget $ f a) ctx ri d
    i <- (sample . current) d
    let combinedEdits = leftmost [updated edit2,updated edit1,updated d]
    combinedEdits' <- holdDyn i combinedEdits
    let combinedHints = leftmost [hint1,hint2]
    return (b,combinedEdits',combinedHints)

context :: MonadWidget t m => EstuaryWidget t m a (Dynamic t Context)
context = EstuaryWidget $ \ctx _ d -> do
  i <- (sample . current) d
  return (ctx, constDyn i, never)

renderInfo :: MonadWidget t m => EstuaryWidget t m a (Dynamic t RenderInfo)
renderInfo = EstuaryWidget $ \_ ri d -> do
  i <- (sample . current) d
  return (ri, constDyn i, never)

initialValue :: MonadWidget t m => EstuaryWidget t m a a
initialValue = EstuaryWidget $ \_ _ d -> do
  i <- (sample . current) d
  return (i, constDyn i, never)

delta :: MonadWidget t m => EstuaryWidget t m a (Dynamic t a)
delta = EstuaryWidget $ \_ _ d -> do
  i <- (sample . current) d
  return (d, constDyn i, never)

edit :: MonadWidget t m => Dynamic t a -> EstuaryWidget t m a ()
edit x = EstuaryWidget $ \_ _ d -> do
  i <- (sample . current) d
  d' <- holdDyn i $ updated x
  return ((),d',never)

hint :: MonadWidget t m => Event t Hint -> EstuaryWidget t m a ()
hint x = EstuaryWidget $ \_ _ d -> do
  i <- (sample . current) d
  return ((),constDyn i,x)

reflex :: MonadWidget t m => m b -> EstuaryWidget t m a b
reflex m = EstuaryWidget $ \_ _ d -> do
  b <- m
  i <- (sample . current) d
  return (b,constDyn i,never)

embed :: MonadWidget t m => (c -> a) -> EstuaryWidget t m a b -> EstuaryWidget t m c (Dynamic t a)
embed f w = EstuaryWidget $ \ctx ri deltaC -> do
  deltaA <- mapDyn f deltaC
  (_,editsA,hints) <- (widget w) ctx ri deltaA
  i <- (sample . current) deltaC
  return (editsA,constDyn i,hints)

embed' :: MonadWidget t m => (c -> a) -> EstuaryWidget t m a b -> EstuaryWidget t m c (b,Dynamic t a)
embed' f w = EstuaryWidget $ \ctx ri deltaC -> do
  deltaA <- mapDyn f deltaC
  (b,editsA,hints) <- (widget w) ctx ri deltaA
  i <- (sample . current) deltaC
  return ((b,editsA),constDyn i,hints)

-- | We provide two "Applicative-esque" operators that are particularly useful for combining
-- edits from sub-widgets in the edit values produced a widget. <$$> and <**> are very much like
-- <$> and <*> except specialized to EstuaryWidget computations containing Dynamics.

(<**>) :: MonadWidget t m
  => EstuaryWidget t m z (Dynamic t (a -> b))
  -> EstuaryWidget t m z (Dynamic t a)
  -> EstuaryWidget t m z (Dynamic t b)
f <**> a = do
  f' <- f
  a' <- a
  reflex $ combineDyn ($) f' a'

(<$$>) :: MonadWidget t m
  => (a -> b)
  -> EstuaryWidget t m z (Dynamic t a)
  -> EstuaryWidget t m z (Dynamic t b)
f <$$> a = a >>= (reflex . mapDyn f)

-- examples:
{-
textArea' :: MonadWidget t m => Dynamic t String -> m (Dynamic t String)
textArea' x = (sample . current) x >>= return . constDyn -- ...this is just to compile-check the examples below...

example1 :: MonadWidget t m => EstuaryWidget t m String ()
example1 = delta >>= (reflex . textArea') >>= edit

example2 :: MonadWidget t m => EstuaryWidget t m (String,String) ()
example2 = do
  let x = (reflex $ text "editor1") >> embed fst example1
  let y = (reflex $ text "editor2") >> embed snd example1
  edit =<< (\a b -> (a,b)) <$$> x <**> y
-}
