{-# LANGUAGE OverloadedStrings #-}
module Estuary.Widgets.Editor where

import Reflex
import Reflex.Dom
import Data.Text as T
import Control.Monad.IO.Class

import Estuary.Types.Context
import Estuary.Types.RenderInfo
import Estuary.Types.Hint
import Estuary.Types.Variable

-- In this module, we define a type that represents the greater majority of widgets
-- in the Estuary project. Such Editor widgets have access to shared information about the
-- "Context" (user prefs, etc) and render engine (eg. audio and load levels) and are
-- able to propagate Hint-s upwards as necessary, alongside returning some value (often a Variable).

data Editor t m a = Editor {
  runEditor :: Dynamic t Context -> Dynamic t RenderInfo -> m (a, Event t [Hint])
  }

askContext :: MonadWidget t m => Editor t m (Dynamic t Context)
askContext = Editor (\ctx _ -> return (ctx,never))

askRenderInfo :: MonadWidget t m => Editor t m (Dynamic t RenderInfo)
askRenderInfo = Editor (\_ ri -> return (ri,never))

-- Usually we just have a single Hint to issue...
hint :: MonadWidget t m => Event t Hint -> Editor t m ()
hint x = Editor (\_ _ -> return ((),fmap (:[]) x))

-- But if we do have multiple simultaneous Hint-s that is no problem too
hints :: MonadWidget t m => Event t [Hint] -> Editor t m ()
hints xs = Editor (\_ _ -> return ((),xs))

-- Generic Reflex widget code can be "lifted" into Editor with 'liftR'...
liftR :: MonadWidget t m => m a -> Editor t m a
liftR x = Editor (\_ _ -> do
  a <- x
  return (a,never)
  )

liftR2 :: MonadWidget t m => (m (a,Event t [Hint]) -> m (a,Event t [Hint])) -> Editor t m a -> Editor t m a
liftR2 r x = Editor (\ctx ri -> r $ runEditor x ctx ri)

initialValueOfDyn :: MonadWidget t m => Dynamic t a -> Editor t m a
initialValueOfDyn = liftR . sample . current

returnVariable :: MonadWidget t m => Dynamic t a -> Event t a -> Editor t m (Variable t a)
returnVariable deltasDown editsUp = do
  i <- initialValueOfDyn deltasDown
  val <- liftR $ holdDyn i $ leftmost [editsUp, updated deltasDown]
  return $ Variable val editsUp

-- However, an especially common pattern is needing to incorporate Reflex widget
-- code for Variables (in the Estuary sense, ie. things that can be changed either
-- by local editing or network actions). 'reflexWidgetToEditor' provides a convenient way
-- of lifting Reflex code (a -> Event t a -> m (Event t a)) into Editor t m:

reflexWidgetToEditor :: MonadWidget t m => Dynamic t a -> (a -> Event t a -> m (Event t a, Event t [Hint])) -> Editor t m (Variable t a)
reflexWidgetToEditor delta widget = do -- in ReaderT
  i <- initialValueOfDyn delta
  (editEvents,hs) <- liftR $ widget i $ updated delta
  hints hs
  returnVariable delta editEvents

-- Naturally we provide Functor, Applicative, Monad, and MonadIO instances...

instance MonadWidget t m => Functor (Editor t m) where
  fmap f x = Editor (\ctx ri -> do
    (a,hs) <- runEditor x ctx ri
    return (f a,hs)
    )

instance MonadWidget t m => Applicative (Editor t m) where
  pure x = Editor (\_ _ -> return (x,never))
  f <*> x = Editor (\ctx ri -> do
    (aF,hsF) <- runEditor f ctx ri
    (aX,hsX) <- runEditor x ctx ri
    let hs = mergeWith (++) [hsF,hsX]
    return (aF aX, hs)
    )

instance MonadWidget t m => Monad (Editor t m) where
  x >>= f = Editor (\ctx ri -> do
    (aX,hsX) <- runEditor x ctx ri
    (aY,hsY) <- runEditor (f aX) ctx ri
    let hs = mergeWith (++) [hsX,hsY]
    return (aY, hs)
    )

instance MonadWidget t m => MonadIO (Editor t m) where
  liftIO x = liftR $ liftIO x

dynEditor :: MonadWidget t m => Dynamic t (Editor t m a) -> Editor t m (Dynamic t a)
dynEditor dynWidgets = Editor (\ctx ri -> do
  initialWidget <- sample $ current dynWidgets
  let widgetUpdates = updated dynWidgets
  let initialWidget' = runEditor initialWidget ctx ri
  let widgetUpdates' = fmap (\x -> runEditor x ctx ri) widgetUpdates
  theWidget <- widgetHold initialWidget' widgetUpdates' -- Dynamic t (a, Event t [Hint])
  let a = fmap fst theWidget
  let hs = switchPromptlyDyn $ fmap snd theWidget
  return (a, hs)
  )


{- examples:

-- using reflexVariable to include "plain" Reflex widget code
stringEditor :: MonadWidget t m => Dynamic t String -> Editor t m (Variable t String)
stringEditor delta = do
  ctx <- getContext -- eg. so that we could use ctx in the below if we wanted to...
  rInfo <- getRenderInfo -- eg. so that we could use rInfo in the below if we wanted to...
  reflexVariable delta $ \i d -> do -- nb: in Reflex' m now...
    let attrs = constDyn $ ("class" =: "textInputToEndOfLine coding-textarea primary-color code-font")
    x <- textInput $ def & textInputConfig_setValue .~ (fmap T.pack d) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ (T.pack i)
    return $ fmap T.unpack $ _textInput_input x

-- taking advantage of Variable's Applicative instance to easily build widgets for more complex types
compositeEditor :: MonadWidget t m => Dynamic t (String,String) -> Editor t m (Variable t (String,String))
compositeEditor delta = do
  x <- stringEditor $ fmap fst delta
  y <- stringEditor $ fmap snd delta
  return $ (\x y -> (x,y)) <$> x <*> y

-- eg. temporarily integrating into old Estuary expectations with minimal disturbance, using runEditor
compositeEditor' :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Dynamic t (String,String) -> m (Dynamic t (String,String),Event t (String,String),Event t [Hint])
compositeEditor' ctx ri delta = do
  (x,hs) <- runEditor (compositeEditor delta) ctx ri  -- x :: Variable t (String,String)
  return (editorDyn x,editorEdit x,hs)

-}
