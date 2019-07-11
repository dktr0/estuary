{-# LANGUAGE OverloadedStrings #-}
module Estuary.Widgets.EstuaryWidget where

import Reflex
import Reflex.Dom
import Control.Monad.Reader

import Estuary.Types.Context
import Estuary.RenderInfo
import Estuary.Types.Hint


data Editor t a = Editor {
  editorDyn :: Dynamic t a,
  editorEdit :: Event t a
  }

instance Reflex t => Functor (Editor t) where
  fmap f (Editor d e) = Editor (fmap f d) (fmap f e)

instance Reflex t => Applicative (Editor t) where
  pure x = Editor (constDyn x) never
  (Editor fDyn fEdit) <*> (Editor xDyn xEdit) = Editor d e
    where
     d = fDyn <*> xDyn
     e = tagPromptlyDyn d $ leftmost [() <$ fEdit,() <$ xEdit]

-- not sure if we can define a monad instance for Editor
-- that is probably okay for now? Even just Applicative is quite useful in the
-- examples below.


type EstuaryWidget t m = ReaderT (Dynamic t Context,Dynamic t RenderInfo) m

getContext :: MonadWidget t m => EstuaryWidget t m (Dynamic t Context)
getContext = fst <$> ask

getRenderInfo :: MonadWidget t m => EstuaryWidget t m (Dynamic t RenderInfo)
getRenderInfo = snd <$> ask

runEstuaryWidget :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> EstuaryWidget t m a -> m a
runEstuaryWidget ctx ri w = runReaderT w (ctx,ri)

liftReflexEditor :: MonadWidget t m => Dynamic t a -> (a -> Event t a -> m (Event t a)) -> EstuaryWidget t m (Editor t a)
liftReflexEditor delta widget = do -- in ReaderT
  iVal <- lift $ (sample . current) delta
  editEvents <- lift $ widget iVal $ updated delta
  val <- lift $ holdDyn iVal $ leftmost [editEvents,updated delta]
  return $ Editor val editEvents

{- examples:

-- using liftReflexEditor to include "plain" Reflex widget code
stringEstuaryWidget :: MonadWidget t m => Dynamic t String -> EstuaryWidget t m (Editor t String)
stringEstuaryWidget delta = do
  ctx <- getContext -- eg. so that we could use ctx in the below if we wanted to...
  rInfo <- getRenderInfo -- eg. so that we could use rInfo in the below if we wanted to...
  liftReflexEditor delta $ \i d -> do -- nb: in Reflex' m now...
    let attrs = constDyn $ ("class" =: "textInputToEndOfLine coding-textarea primary-color code-font")
    x <- textInput $ def & textInputConfig_setValue .~ (fmap T.pack d) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ (T.pack i)
    return $ fmap T.unpack $ _textInput_input x

-- taking advantage of Editor's Applicative instance to easily build widgets for more complex types
compositeEstuaryWidget :: MonadWidget t m => Dynamic t (String,String) -> EstuaryWidget t m (Editor t (String,String))
compositeEstuaryWidget delta = do
  x <- stringEstuaryWidget $ fmap fst delta
  y <- stringEstuaryWidget $ fmap snd delta
  return $ (\x y -> (x,y)) <$> x <*> y

-- eg. temporarily integrating into old Estuary expectations with minimal disturbance...
compositeEstuaryWidget' :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Dynamic t (String,String) -> m (Dynamic t (String,String),Event t (String,String))
compositeEstuaryWidget' ctx ri delta = do
  x <- runEstuaryWidget ctx ri (compositeEstuaryWidget delta) -- x :: Editor t (String,String)
  return (editorDyn x,editorEdit x)

-- *** next: need to add Hints to all of this

-}
