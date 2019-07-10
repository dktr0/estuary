{-# LANGUAGE OverloadedStrings #-}
module Estuary.Widgets.EstuaryWidget where

import Reflex
import Reflex.Dom

import Estuary.Types.Context
import Estuary.RenderInfo
import Estuary.Types.Hint


data Editor t a = Editor {
  editorDyn :: Dynamic t a,
  editorEdit :: Event t a
  }

instance Functor (Editor t) where
  fmap f (Editor d e) = Editor (fmap x d) (fmap d e)

instance Applicative (Editor t) where
  pure x = Editor (constDyn x) never
  (Editor fDyn fEdit) <*> (Editor xDyn xEdit) = Editor d e
    where
     d = fDyn <*> xDyn
     e = tagPromptlyDyn d $ leftmost [fEdit,xEdit]

-- not sure if we can define a monad instance for Editor
-- that is probably okay for now? Even just Applicative is quite useful in the
-- examples below.


type Widget t m = ReaderT (Dynamic t Context,Dynamic t RenderInfo) m

getContext :: Widget t m (Dynamic t Context)
getContext = fst <$> ask

getRenderInfo :: Widget t m (Dynamic t RenderInfo)
getRenderInfo = snd <$> ask

runWidget :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Widget t m a -> m a
runWidget ctx ri w = runReaderT w (ctx,ri)

liftReflexEditor :: MonadWidget t m => Dynamic t a -> (a -> Event t a -> m (Event t a)) -> Widget t m (Editor t a)
liftReflexEditor delta widget = do
  iVal <- (sample . current) delta
  editEvents <- widget iVal $ updated delta
  holdDyn ival $ leftmost [editEvents,updated delta]


{- examples:

-- using liftReflexEditor to include "plain" Reflex widget code
stringWidget :: MonadWidget t m => Dynamic t String -> Widget t m (Editor t String)
stringWidget delta = do
  ctx <- getContext -- eg. so that we could use ctx in the below if we wanted to...
  rInfo <- getRenderInfo -- eg. so that we could use rInfo in the below if we wanted to...
  liftReflexEditor delta $ \i d -> do -- nb: in Reflex' m now...
    let attrs = constDyn $ ("class" =: "textInputToEndOfLine coding-textarea primary-color code-font")
    x <- textInput $ def & textInputConfig_setValue .~ (fmap T.pack d) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ (T.pack i)
    return $ fmap T.unpack $ _textInput_input x

-- taking advantage of Editor's Applicative instance to easily build widgets for more complex types
compositeWidget :: MonadWidget t m => Dynamic t (String,String) -> Widget t m (Editor t (String,String))
compositeWidget delta = do
  x <- stringWidget $ fmap fst delta
  y <- stringWidget $ fmap snd delta
  return $ (\x y -> (x,y)) <$> x <*> y

-- eg. temporarily integrating into old Estuary expectations with minimal disturbance...
compositeWidget' :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Dynamic t (String,String) -> m (Dynamic t (String,String),Event t (String,String))
compositeWidget' ctx ri delta = do
  x <- runWidget ctx ri (compositeWidget delta) -- x :: Editor t (String,String)
  return (editorDyn x,editorEdit x)

-- *** next: need to add Hints to all of this

-}
