{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.Sound where

import Estuary.Reflex.Utility
import Reflex
import Reflex.Dom
import Data.Map
import Text.Read(readMaybe)
import Data.Maybe
import Control.Monad
import GHCJS.DOM.EventM
import Estuary.Tidal.Types
import Estuary.Widgets.Generic

data ContainerSignal = Flash deriving(Eq, Show)


selectableWidget::MonadWidget t m => Sound -> Event t ContainerSignal -> m (Dynamic t (Sound, Event t GenericSignal))
selectableWidget iVal ev = mdo
  (element,_) <- elDynAttr' "div" attrs $ text "Click here to delete me"
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  mouseOver <- wrapDomEvent (_el_element element) (onEventName Mouseover) (mouseXY)
  mouseOut <- wrapDomEvent (_el_element element) (onEventName Mouseout) (mouseXY)
  let mouse = leftmost [mouseOut,mouseOver]
  attrsToggle <- toggle (False) mouse
  attrs <- forDyn attrsToggle (\k -> if k then "style"=:"background-color:green" else empty)
  let event' = fmap (\k->DeleteMe) clickEv
  let event'' = fmap (\k->"eventFired") clickEv
  string <- holdDyn "test" event''
  display string
  return $ constDyn (Sound $ Just $ Sample "one" 1 1 False, event')

errorMessageWidget::MonadWidget t m => Sound -> Event t ContainerSignal ->  m (Dynamic t (Sound, Event t GenericSignal))
errorMessageWidget a b = mdo
  (element,result) <- elDynAttr' "div" attrs $ mdo
    sampleTextField <- textInput def
    let sampleName = _textInput_value sampleTextField
    sampleNumberField <- textInput $ def & textInputConfig_inputType .~"number"
    let sampleN = _textInput_value sampleNumberField
    repeatNumberField <- textInput $ def & textInputConfig_inputType .~"number"
    let repeats = _textInput_value repeatNumberField
    degradeBox <- checkbox False def
    let degradeVal = _checkbox_value degradeBox
    sampleInfo'' <- combineDyn (\name num-> (name,num)) sampleName sampleN --Dyn (string,string..)
    sampleInfo' <- combineDyn (\reps degs ->(reps,degs)) repeats degradeVal
    sampleInfo <- combineDyn (\(a,b) (c,d)->(a,b,c,d)) sampleInfo'' sampleInfo'
    validSample <- forDyn sampleInfo validateSample' --Dynamic Either Sample String
    errorMessage <- forDyn validSample (either (\_->"") (id))
    showErrorMessage <- combineDyn (\tog msg-> if tog then msg else "") mouseOverToggle errorMessage --Dynamic String
    sound <- forDyn validSample (either (Sound . Just) (\_->Sound Nothing))--Dynamic Sound
    deleteButton <- liftM (DeleteMe <$) $ button "-"
    display sound
    dynText showErrorMessage
    combineDyn (\s err->(s,deleteButton,err)) sound errorMessage
  mouseOver <- wrapDomEvent (_el_element element) (onEventName Mouseover) (mouseXY)
  mouseOut <- wrapDomEvent (_el_element element) (onEventName Mouseout) (mouseXY)
  let mouse = leftmost [mouseOut,mouseOver]
  mouseOverToggle <- toggle False mouse
  attrs <- forDyn result (\(_,_,validMsg) -> if validMsg=="" then "style"=:"background-color:lightgreen" else "style"=:"background-color:yellow")
  forDyn result (\(a,b,_)->(a,b))



textWidget::MonadWidget t m => Sound -> Event t ContainerSignal ->  m (Dynamic t (Sound, Event t GenericSignal))
textWidget _ _= el "div" $ mdo
  sampleTextField <- textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"border-color:green")
  let sampleName = _textInput_value sampleTextField
  sampleNumberField <- textInput $ def & textInputConfig_inputType .~"number" & textInputConfig_attributes .~ sampleNAttrs
  let sampleN = _textInput_value sampleNumberField
  repeatNumberField <- textInput $ def & textInputConfig_inputType .~"number" & textInputConfig_attributes .~ repeatsAttrs
  let repeats = _textInput_value repeatNumberField
  degradeByField <-textInput $ def & textInputConfig_attributes .~ degradeByAttrs
  let degradeByNum = _textInput_value degradeByField
  sampleInfo'' <- combineDyn (\name num-> (name,num)) sampleName sampleN --Dyn (string,string..)
  sampleInfo' <- combineDyn (\reps degs ->(reps,degs)) repeats degradeByNum
  sampleInfo <- combineDyn (\(a,b) (c,d)->(a,b,c,d)) sampleInfo'' sampleInfo'
  sampleNAttrs <- forDyn sampleN (\k->if k=="" || isJust (readMaybe k::Maybe Int) then valid else invalid)
  repeatsAttrs <- forDyn repeats (\k->if k=="" || isJust (readMaybe k::Maybe Int) then valid else invalid)
  degradeByAttrs <- forDyn degradeByNum (\k->if k=="" || isJust (readMaybe k::Maybe Bool) then valid else invalid)
  validSample <- forDyn sampleInfo (validateSample) --Dynamic Maybe Sample
  sound <- forDyn validSample Sound--Dynamic Sound
  deleteButton <- liftM (DeleteMe <$) $ button "-"
  display sound
  forDyn sound (\k->(k,deleteButton))
  where valid = "style"=:"border-color:green"
        invalid = "style"=:"border-color:red"




validateSample::(String,String,String,String) -> Maybe Sample
validateSample (name,num,repeats,deg) = if allCheck then
   Just $ Sample name (if num=="" then 0 else read num::Int) (if repeats =="" then 1 else read repeats::Int) (if deg=="" then False else read deg::Bool)
   else Nothing
 where
   nameCheck = Just name
   numCheck = if num=="" then Just 0 else readMaybe num::Maybe Int
   repeatCheck = if repeats =="" then Just 1 else readMaybe repeats::Maybe Int
   degradeCheck = if deg =="" then Just False else readMaybe deg::Maybe Bool
   allCheck = isJust nameCheck && isJust numCheck && isJust repeatCheck && isJust degradeCheck



validateSample'::(String,String,String,Bool) -> Either Sample String
validateSample' (name,num,repeats,deg) = if allCheck then
  Left $ Sample name (if num=="" then 0 else read num::Int) (if repeats =="" then 1 else read repeats::Int) deg
  else
    Right $ Prelude.foldl (\a (b,str)-> if (not b) then a++str++", " else a) "Invalid: " errorMessages
  where
    nameCheck = Just name
    numCheck = if num=="" then Just 0 else readMaybe num::Maybe Int
    repeatCheck = if repeats =="" then Just 1 else readMaybe repeats::Maybe Int
    degradeCheck = Just deg
    allCheck = isJust nameCheck && isJust numCheck && isJust repeatCheck && isJust degradeCheck
    errorMessages = [(isJust nameCheck,"sample name"),(isJust numCheck,"sample number"),(isJust repeatCheck,"reapeats"),(isJust degradeCheck,"degradeBy value")]
