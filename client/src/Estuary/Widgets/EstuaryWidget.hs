{-# LANGUAGE OverloadedStrings #-}
module Estuary.Widgets.EstuaryWidget where

import Reflex
import Reflex.Dom
import Control.Monad.Reader
import Data.Text as T -- note: just for examples

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


data EstuaryWidget t m a = EstuaryWidget {
  runEstuaryWidget :: Dynamic t Context -> Dynamic t RenderInfo -> m (a, Event t [Hint])
  }

getContext :: MonadWidget t m => EstuaryWidget t m (Dynamic t Context)
getContext = EstuaryWidget (\ctx _ -> return (ctx,never))

getRenderInfo :: MonadWidget t m => EstuaryWidget t m (Dynamic t RenderInfo)
getRenderInfo = EstuaryWidget (\_ ri -> return (ri,never))

hint :: MonadWidget t m => Event t Hint -> EstuaryWidget t m ()
hint x = EstuaryWidget (\_ _ -> return ((),fmap (:[]) x))

hints :: MonadWidget t m => Event t [Hint] -> EstuaryWidget t m ()
hints xs = EstuaryWidget (\_ _ -> return ((),xs))

reflex :: MonadWidget t m => m a -> EstuaryWidget t m a
reflex x = EstuaryWidget (\_ _ -> do
  a <- x
  return (a,never)
  )

reflexEditor :: MonadWidget t m => Dynamic t a -> (a -> Event t a -> m (Event t a)) -> EstuaryWidget t m (Editor t a)
reflexEditor delta widget = do -- in ReaderT
  iVal <- reflex $ (sample . current) delta
  editEvents <- reflex $ widget iVal $ updated delta
  val <- reflex $ holdDyn iVal $ leftmost [editEvents,updated delta]
  return $ Editor val editEvents

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


{- examples:

-- using reflexEditor to include "plain" Reflex widget code
stringEstuaryWidget :: MonadWidget t m => Dynamic t String -> EstuaryWidget t m (Editor t String)
stringEstuaryWidget delta = do
  ctx <- getContext -- eg. so that we could use ctx in the below if we wanted to...
  rInfo <- getRenderInfo -- eg. so that we could use rInfo in the below if we wanted to...
  reflexEditor delta $ \i d -> do -- nb: in Reflex' m now...
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
compositeEstuaryWidget' :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Dynamic t (String,String) -> m (Dynamic t (String,String),Event t (String,String),Event t [Hint])
compositeEstuaryWidget' ctx ri delta = do
  (x,hs) <- runEstuaryWidget (compositeEstuaryWidget delta) ctx ri  -- x :: Editor t (String,String)
  return (editorDyn x,editorEdit x,hs)

-}
