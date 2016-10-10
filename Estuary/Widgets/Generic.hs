module Estuary.Widgets.Generic where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Control.Monad
import Estuary.Reflex.Utility
import Data.Map

data GenericSignal = Ping | DeleteMe | MakeGroup | MakeLayer | RebuildMe deriving (Eq, Show)

pingButton :: MonadWidget t m => String -> m (Event t GenericSignal)
pingButton label = liftM (Ping <$) $ button label

pingButton' :: MonadWidget t m => String -> m (Dynamic t ((),Event t GenericSignal))
pingButton' label = do
  x <- pingButton label
  return $ constDyn ((),x)

pingButton'' :: MonadWidget t m => String -> a -> b -> m (Dynamic t ((),Event t GenericSignal))
pingButton'' label _ _ = pingButton' label

pingButton''':: MonadWidget t m => String -> Map String String -> a -> b -> m (Dynamic t ((),Event t GenericSignal))
pingButton''' label attrs _ _ = do
  b <- buttonDynAttrs label (Ping) $ constDyn attrs
  return $ constDyn ((), b)

tdPingButton':: MonadWidget t m => String -> Map String String -> a -> b -> m (Dynamic t ((),Event t GenericSignal))
tdPingButton' label attrs _ _ = el "td" $ do
  b <- buttonDynAttrs label (Ping) $ constDyn attrs
  return $ constDyn ((), b)

tdPingButton'':: MonadWidget t m => String -> a -> b -> m (Dynamic t ((),Event t GenericSignal))
tdPingButton'' label _ _ = el "td" $ do
  b <- buttonDynAttrs label (Ping) $ constDyn ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:30px;vertical-align:middle"))
  return $ constDyn ((), b)

clickableDiv :: MonadWidget t m => String -> m (Event t ())
clickableDiv label = do
  (element,_) <- elAttr' "div" attr $ text label
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ (() <$) clickEv
  where
    attr = singleton "style" "background-color: gray; display: inline;"

clickableDivAttrs::MonadWidget t m => String -> a -> Map String String -> m (Event t a)
clickableDivAttrs label val attrs= do
  (element,_) <- elAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ (val <$) clickEv

clickableDivAttrs'::MonadWidget t m => String -> a -> Map String String -> x -> y -> m (Dynamic t ((),Event t a))
clickableDivAttrs' label val attrs _ _= do
  (element,_) <- elAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  let event = (val <$) clickEv
  return $ constDyn ((),event)

clickableDiv' :: MonadWidget t m => String -> a -> m (Event t a)
clickableDiv' label e = liftM (e <$) $ clickableDiv label

pingDiv :: MonadWidget t m => String -> m (Event t GenericSignal)
pingDiv label = clickableDiv' label Ping

pingDiv' :: MonadWidget t m => String -> m (Dynamic t ((),Event t GenericSignal))
pingDiv' label = do
  x <- pingDiv label
  return $ constDyn ((),x)
