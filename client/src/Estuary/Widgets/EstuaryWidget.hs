{-# LANGUAGE OverloadedStrings #-}
module Estuary.Widgets.EstuaryWidget where

import Reflex
import Reflex.Dom
import Data.Text as T -- note: just for examples

import Estuary.Types.Context
import Estuary.RenderInfo
import Estuary.Types.Hint
import Estuary.Types.Variable

-- In this module, we define a type that represents the greater majority of widgets
-- in the Estuary project. Such widgets have access to shared information about the
-- "Context" (user prefs, etc) and render engine (eg. audio and load levels) and are
-- able to propagate Hint-s upwards as necessary, alongside returning some value.

data EstuaryWidget t m a = EstuaryWidget {
  runEstuaryWidget :: Dynamic t Context -> Dynamic t RenderInfo -> m (a, Event t [Hint])
  }

askContext :: MonadWidget t m => EstuaryWidget t m (Dynamic t Context)
askContext = EstuaryWidget (\ctx _ -> return (ctx,never))

askRenderInfo :: MonadWidget t m => EstuaryWidget t m (Dynamic t RenderInfo)
askRenderInfo = EstuaryWidget (\_ ri -> return (ri,never))

-- Usually we just have a single Hint to issue...
hint :: MonadWidget t m => Event t Hint -> EstuaryWidget t m ()
hint x = EstuaryWidget (\_ _ -> return ((),fmap (:[]) x))

-- But if we do have multiple simultaneous Hint-s that is no problem too
hints :: MonadWidget t m => Event t [Hint] -> EstuaryWidget t m ()
hints xs = EstuaryWidget (\_ _ -> return ((),xs))

-- Generic Reflex widget code can be "lifted" into EstuaryWidget with 'reflex'...
reflex :: MonadWidget t m => m a -> EstuaryWidget t m a
reflex x = EstuaryWidget (\_ _ -> do
  a <- x
  return (a,never)
  )

liftR2 :: MonadWidget t m => (m (a,Event t [Hint]) -> m (a,Event t [Hint])) -> EstuaryWidget t m a -> EstuaryWidget t m a
liftR2 r x = EstuaryWidget (\ctx ri -> r $ runEstuaryWidget x ctx ri)


-- However, an especially common pattern is needing to incorporate Reflex widget
-- code for Variables (in the Estuary sense, ie. things that can be changed either
-- by local editing or network actions). 'reflexVariable' provides a convenient way
-- of lifting Reflex code (a -> Event t a -> m (Event t a)) into EstuaryWidget t m:

reflexVariable :: MonadWidget t m => Dynamic t a -> (a -> Event t a -> m (Event t a, Event t [Hint])) -> EstuaryWidget t m (Variable t a)
reflexVariable delta widget = do -- in ReaderT
  iVal <- reflex $ (sample . current) delta
  (editEvents,hs) <- reflex $ widget iVal $ updated delta
  val <- reflex $ holdDyn iVal $ leftmost [editEvents,updated delta]
  hints hs
  return $ Variable val editEvents

-- Naturally we provide Functor, Applicative, and Monad instances
-- *** should see if we also can provide MonadIO ***

instance MonadWidget t m => Functor (EstuaryWidget t m) where
  fmap f x = EstuaryWidget (\ctx ri -> do
    (a,hs) <- runEstuaryWidget x ctx ri
    return (f a,hs)
    )

instance MonadWidget t m => Applicative (EstuaryWidget t m) where
  pure x = EstuaryWidget (\_ _ -> return (x,never))
  f <*> x = EstuaryWidget (\ctx ri -> do
    (aF,hsF) <- runEstuaryWidget f ctx ri
    (aX,hsX) <- runEstuaryWidget x ctx ri
    let hs = mergeWith (++) [hsF,hsX]
    return (aF aX, hs)
    )

instance MonadWidget t m => Monad (EstuaryWidget t m) where
  x >>= f = EstuaryWidget (\ctx ri -> do
    (aX,hsX) <- runEstuaryWidget x ctx ri
    (aY,hsY) <- runEstuaryWidget (f aX) ctx ri
    let hs = mergeWith (++) [hsX,hsY]
    return (aY, hs)
    )

dynEstuaryWidget :: MonadWidget t m => Dynamic t (EstuaryWidget t m a) -> EstuaryWidget t m (Dynamic t a)
dynEstuaryWidget dynWidgets = EstuaryWidget (\ctx ri -> do
  initialWidget <- sample $ current dynWidgets
  let widgetUpdates = updated dynWidgets
  let initialWidget' = runEstuaryWidget initialWidget ctx ri
  let widgetUpdates' = fmap (\x -> runEstuaryWidget x ctx ri) widgetUpdates
  theWidget <- widgetHold initialWidget' widgetUpdates' -- Dynamic t (a, Event t [Hint])
  let a = fmap fst theWidget
  let hs = switchPromptlyDyn $ fmap snd theWidget
  return (a, hs)
  )


{- examples:

-- using reflexVariable to include "plain" Reflex widget code
stringEstuaryWidget :: MonadWidget t m => Dynamic t String -> EstuaryWidget t m (Variable t String)
stringEstuaryWidget delta = do
  ctx <- getContext -- eg. so that we could use ctx in the below if we wanted to...
  rInfo <- getRenderInfo -- eg. so that we could use rInfo in the below if we wanted to...
  reflexVariable delta $ \i d -> do -- nb: in Reflex' m now...
    let attrs = constDyn $ ("class" =: "textInputToEndOfLine coding-textarea primary-color code-font")
    x <- textInput $ def & textInputConfig_setValue .~ (fmap T.pack d) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ (T.pack i)
    return $ fmap T.unpack $ _textInput_input x

-- taking advantage of Variable's Applicative instance to easily build widgets for more complex types
compositeEstuaryWidget :: MonadWidget t m => Dynamic t (String,String) -> EstuaryWidget t m (Variable t (String,String))
compositeEstuaryWidget delta = do
  x <- stringEstuaryWidget $ fmap fst delta
  y <- stringEstuaryWidget $ fmap snd delta
  return $ (\x y -> (x,y)) <$> x <*> y

-- eg. temporarily integrating into old Estuary expectations with minimal disturbance, using runEstuaryWidget
compositeEstuaryWidget' :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Dynamic t (String,String) -> m (Dynamic t (String,String),Event t (String,String),Event t [Hint])
compositeEstuaryWidget' ctx ri delta = do
  (x,hs) <- runEstuaryWidget (compositeEstuaryWidget delta) ctx ri  -- x :: Variable t (String,String)
  return (editorDyn x,editorEdit x,hs)

-}
