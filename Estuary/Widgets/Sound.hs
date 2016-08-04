{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.Sound where

import Reflex
import Reflex.Dom
import Data.Map
import Text.Read(readMaybe)
import Data.Maybe
import Control.Monad

import Estuary.Tidal.Types

data ContainerSignal = Flash deriving(Eq, Show)
data ChildSignal = DeleteMe deriving (Eq, Show)



textWidget::MonadWidget t m => Sound -> Event t ContainerSignal ->  m (Dynamic t (Sound, Event t ChildSignal))
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
  changeSoundButton <- button "Change Sound"
  sound <- forDyn validSample Sound--Dynamic Sound
  deleteButton <- liftM (DeleteMe <$) $ button "-"
  let changeSound = tagDyn sound changeSoundButton -- ::MonadWidget t m=> m(Event t Sound) --Event Sound
  sound' <- holdDyn (Sound Nothing) changeSound
  display sound'
  forDyn sound' (\k->(k,deleteButton))
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
