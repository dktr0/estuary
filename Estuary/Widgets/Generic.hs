{-# LANGUAGE RecursiveDo #-}


module Estuary.Widgets.Generic where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Control.Monad
import Estuary.Reflex.Utility
import Data.Map
import Data.List
import Estuary.Tidal.Types
import Estuary.WebDirt.Foreign
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal.Pure as P


data Hint = SampleHint String deriving (Eq)

doHint :: T.JSVal -> Hint -> IO ()
doHint wd (SampleHint x) = sampleHint wd (P.pToJSVal x)

data EditSignal a = ChangeValue a | MakeNew | Close | DeleteMe | RepDiv | MakeGroup | MakeLayer
 | RebuildMe | MakeL3 | MakeL4 | MakeRepOrDiv | Eval deriving (Eq)

data Context = L4 | L3 deriving (Eq,Show)

instance Show a => Show (EditSignal a) where
  show (ChangeValue a) =  show a
  show MakeRepOrDiv = "* or /"
  show Close = "close"
  show DeleteMe = "delete"
  show MakeGroup = "[  ]"
  show MakeLayer = "[,,]"
  show RebuildMe = "RebuildMe"
  show MakeL3 = "L3"
  show MakeL4 = "L4"
  show Eval = "eval"

isChangeValue::EditSignal a -> Bool
isChangeValue (ChangeValue _) = True
isChangeValue _ = False
--data EditSignal = DeleteMe | MakeGroup |


clickableDiv :: MonadWidget t m => String -> m (Event t ())
clickableDiv label = do
  (element,_) <- elAttr' "div" attr $ text label
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ (() <$) clickEv
  where
    attr = singleton "style" "background-color: gray; display: inline;"

clickableDiv' :: MonadWidget t m => String -> a -> m (Event t a)
clickableDiv' label e = liftM (e <$) $ clickableDiv label

clickableDivClass :: MonadWidget t m => String -> String -> m (Event t ())
clickableDivClass label c = do
  (element,_) <- elAttr' "div" (singleton "class" c) $ text label
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ (() <$) clickEv

clickableDivClass' :: MonadWidget t m => String -> String -> a -> m (Event t a)
clickableDivClass' label c e = liftM (e <$) $ clickableDivClass label c

-- with displayed text that can change
clickableDivClass'':: MonadWidget t m => Dynamic t String -> String -> a -> m (Event t a)
clickableDivClass'' label c e = do
  (element, _) <- elAttr' "div" ("class"=:c) $ dynText label
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ (e <$) clickEv

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


pingButton :: MonadWidget t m => String -> m (Event t ())
pingButton label = liftM (() <$) $ button label

pingButton' :: MonadWidget t m => String -> m (Dynamic t ((),Event t ()))
pingButton' label = do
  x <- pingButton label
  return $ constDyn ((),x)

pingButton'' :: MonadWidget t m => String -> a -> b -> m (Dynamic t ((),Event t ()))
pingButton'' label _ _ = pingButton' label

pingButton''':: MonadWidget t m => String -> Map String String -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())))
pingButton''' label attrs _ _ = do
  b <- buttonDynAttrs label (ChangeValue ()) $ constDyn attrs
  return $ constDyn ((), b)

makeNewButton:: (MonadWidget t m)=> String -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())) )
makeNewButton label _ _ = do
  a <- button label
  return $ constDyn ((), ((MakeNew::EditSignal ()) <$) a)

pingDiv :: MonadWidget t m => String -> m (Event t ())
pingDiv label = clickableDiv' label ()

pingDiv' :: MonadWidget t m => String -> m (Dynamic t ((),Event t ()))
pingDiv' label = do
  x <- pingDiv label
  return $ constDyn ((),x)

tdButtonAttrs:: MonadWidget t m => String -> a -> Map String String -> m (Event t a)
tdButtonAttrs s val attrs = do
  (element, _) <- elAttr' "td" attrs $ text s
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ ((val) <$) clickEv

-- with displayed text that can change
tdButtonAttrs':: MonadWidget t m => Dynamic t String -> a -> Map String String -> m (Event t a)
tdButtonAttrs' s val attrs = do
  (element, _) <- elAttr' "td" attrs $ dynText s
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ ((val) <$) clickEv

tdPingButtonAttrs:: MonadWidget t m => String -> Map String String -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())))
tdPingButtonAttrs label attrs _ _ = el "td" $ do
  b <- buttonDynAttrs label (ChangeValue ()) $ constDyn attrs
  return $ constDyn ((), b)




-- validator.w3.org

-- A clickable td element. Each click cycles to the next element in the map. Updated with a RepOrDiv event.
-- rep/div values get shown on the button too.
--clickListWidget::(MonadWidget t m, Show a, Eq a) => Map Int a ->  GeneralPattern a -> Event t RepOrDiv -> m (Dynamic t (GeneralPattern a, Event t EditSignal))
--clickListWidget cycleMap (Atom iVal iReps) updatedReps = mdo
--  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVal) $ elems cycleMap
--  sampleButton <- tdButtonAttrs' showVal (iVal) $ "class"=:"clickListtd"
--  num <- count sampleButton >>= mapDyn (\x-> (x+initialNum) `mod` length cycleMap)
--  str'' <- mapDyn (\x-> maybe iVal id $ Data.Map.lookup x cycleMap) num
--  let str' = updated str''
--  str <- holdDyn (iVal) str'
--  reps <- holdDyn (iReps) updatedReps
--  returnSample <- combineDyn (\x r -> Atom x r) str reps
--  showVal <- mapDyn show returnSample
--  mapDyn (\x->(x,never)) returnSample
