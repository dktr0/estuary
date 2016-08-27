module Estuary.Reflex.Utility where

import Reflex
import Reflex.Dom
import Control.Monad (liftM)
import Data.Map
import GHCJS.DOM.EventM



-- Anytime an event is received issue another event of a given constant value.

constEvent :: Reflex t => a -> Event t b -> Event t a
constEvent a b = fmap (const a) b

-- Whenever a received event matches a value, issue another event of a given
-- constant value.

matchEvent :: (Reflex t, Eq a) => a -> b -> Event t a -> Event t b
matchEvent a b = fmap (const  b) . ffilter (==a)

-- a button that, instead of producing Event t (), produces an event of
-- some constant value

button' :: (MonadWidget t m) => String -> a -> m (Event t a)
button' t r = do
  x <- button t
  return $ fmap (const r) x

-- Button With Dynamic attributes

buttonDynAttrs :: MonadWidget t m => String -> a -> Dynamic t (Map String String)-> m (Event t a)
buttonDynAttrs s val attrs = do
  (e, _) <- elDynAttr' "button" attrs $ text s
  let event = domEvent Click e
  return $ fmap (const val) event

--td buttons used to create a button out of an html table cell

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

-- for use in containers
tdPingButton:: MonadWidget t m => String -> a -> Map String String -> b -> c -> m (Dynamic t ((),Event t a) )
tdPingButton s val attrs _ _= do
  (element, _) <- elAttr' "td" attrs $ text s
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ constDyn ((), ((val) <$) clickEv )




  --
  -- errorMessageWidget::MonadWidget t m => Sound -> Event t ContainerSignal ->  m (Dynamic t (Sound, Event t GenericSignal))
  -- errorMessageWidget a b = mdo
  --   (element,result) <- elDynAttr' "div" attrs $ mdo
  --     sampleTextField <- textInput def
  --     let sampleName = _textInput_value sampleTextField
  --     sampleNumberField <- textInput $ def & textInputConfig_inputType .~"number"
  --     let sampleN = _textInput_value sampleNumberField
  --     repeatNumberField <- textInput $ def & textInputConfig_inputType .~"number"
  --     let repeats = _textInput_value repeatNumberField
  --     degradeBox <- checkbox False def
  --     let degradeVal = _checkbox_value degradeBox
  --     sampleInfo'' <- combineDyn (\name num-> (name,num)) sampleName sampleN --Dyn (string,string..)
  --     sampleInfo' <- combineDyn (\reps degs ->(reps,degs)) repeats degradeVal
  --     sampleInfo <- combineDyn (\(a,b) (c,d)->(a,b,c,d)) sampleInfo'' sampleInfo'
  --     validSample <- forDyn sampleInfo validateSample' --Dynamic Either Sample String
  --     errorMessage <- forDyn validSample (either (\_->"") (id))
  --     showErrorMessage <- combineDyn (\tog msg-> if tog then msg else "") mouseOverToggle errorMessage --Dynamic String
  --     sound <- forDyn validSample (either (Sound . Just) (\_->Sound Nothing))--Dynamic Sound
  --     deleteButton <- liftM (DeleteMe <$) $ button "-"
  --     display sound
  --     dynText showErrorMessage
  --     combineDyn (\s err->(s,deleteButton,err)) sound errorMessage
  --   mouseOver <- wrapDomEvent (_el_element element) (onEventName Mouseover) (mouseXY)
  --   mouseOut <- wrapDomEvent (_el_element element) (onEventName Mouseout) (mouseXY)
  --   let mouse = leftmost [mouseOut,mouseOver]
  --   mouseOverToggle <- toggle False mouse
  --   attrs <- forDyn result (\(_,_,validMsg) -> if validMsg=="" then "style"=:"background-color:lightgreen" else "style"=:"background-color:yellow")
  --   forDyn result (\(a,b,_)->(a,b))
