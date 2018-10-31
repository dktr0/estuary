{-# LANGUAGE RecursiveDo #-}


module Estuary.Widgets.Generic where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Control.Monad
import Data.Map
import Data.List
import Data.Bool(bool)

import Estuary.Reflex.Utility
import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Reflex.Container
import Data.Maybe
import Text.Read (readMaybe)
import Estuary.WebDirt.Foreign
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal.Pure as P

import Estuary.Types.Hint


data EditSignal a = ChangeValue a | MakeNew | Close | DeleteMe | RepDiv | MakeGroup | MakeLayer
 | RebuildMe | MakeL3 | MakeL4 | MakeRepOrDiv | Eval | DeleteContainer | LayerSplit | TransformMe deriving (Eq)

--data EditSignal a = RebuildMe | PotentialSignal (Potential a)


toPotential::EditSignal a -> Potential a
toPotential (ChangeValue a) = Potential a
toPotential (MakeL3) = PotentialLiveness L3
toPotential (MakeL4) = PotentialLiveness L4
toPotential (Close) = Inert
toPotential (MakeRepOrDiv) = PotentialRepOrDiv
toPotential (MakeGroup) = PotentialMakeGroup
toPotential (MakeLayer) = PotentialMakeLayer
toPotential (DeleteMe) = PotentialDelete


toEditSigGenPat::EditSignal a -> EditSignal (GeneralPattern a)
toEditSigGenPat (ChangeValue a) = ChangeValue (Atom a Inert Once)
toEditSigGenPat (MakeL4) = MakeL4
toEditSigGenPat (MakeL3) = MakeL3
toEditSigGenPat (MakeNew) =MakeNew
toEditSigGenPat (Close) = Close
toEditSigGenPat (DeleteMe) = DeleteMe
toEditSigGenPat (RepDiv) = RepDiv
toEditSigGenPat (MakeGroup) = MakeGroup
toEditSigGenPat (MakeLayer) = MakeLayer
toEditSigGenPat (RebuildMe) = RebuildMe
toEditSigGenPat (MakeRepOrDiv) = MakeRepOrDiv
toEditSigGenPat (Eval) = Eval
toEditSigGenPat (DeleteContainer) = DeleteContainer
toEditSigGenPat (LayerSplit) = LayerSplit

instance Show a => Show (EditSignal a) where
  show (ChangeValue a) =  show a
  show MakeRepOrDiv = "* or /"
  show Close = "close"
  show DeleteMe = "delete"
  show DeleteContainer ="delete container"
  show MakeGroup = "[  ]"
  show MakeLayer = "[,,]"
  show RebuildMe = "RebuildMe"
  show MakeL3 = "L3"
  show MakeL4 = "L4"
  show Eval = "eval"
  show LayerSplit = "LayerSplit"
  show TransformMe = "Transform"


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

mouseOverClickableDiv::MonadWidget t m => Dynamic t String -> String -> a -> m(Event t a)
mouseOverClickableDiv label c e = mdo
  (element, _) <- elDynAttr' "div" attrs $ dynText label
  mouseOver <- liftM (True <$) $ wrapDomEvent (_el_element element) (onEventName Mouseover) mouseXY
  mouseOut <- liftM (False <$) $ wrapDomEvent (_el_element element) (onEventName Mouseout) mouseXY
  isMouseOver <- holdDyn False $ leftmost [mouseOut, mouseOver]
  attrs  <- mapDyn (fromList . (\x-> [("class",c),x]) . ((,) "style") . bool "" ";background-color:rgba(144,238,144,0.2);") isMouseOver
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

-- with displayed text that can change
clickableSpanClass:: MonadWidget t m => Dynamic t String -> String -> a -> m (Event t a)
clickableSpanClass label c e = do
  (element, _) <- elAttr' "span" ("class"=:c) $ dynText label
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ (e <$) clickEv

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

growingTextInput::MonadWidget t m => TextInputConfig t -> m (TextInput t)
growingTextInput config = mdo
  let attrs = _textInputConfig_attributes config
  dynAttrs <- combineDyn (\m w-> insertWith (++) "style" (";width:"++ show (max 20 $ min 100 $ 8*length w) ++ "px" ++";") m) attrs (_textInput_value textField)
  let newConfig = TextInputConfig (_textInputConfig_inputType config) (_textInputConfig_initialValue config) (_textInputConfig_setValue config) dynAttrs
  textField <- textInput newConfig
  return textField

whitespace:: (MonadWidget t m, Show a, Eq a)=> Dynamic t Liveness -> GeneralPattern a -> String -> [EditSignal (GeneralPattern a)] -> () -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t ((), Event t (EditSignal (GeneralPattern a)), Event t Hint))
whitespace liveness iVal cssClass popupList _ event = elAttr "div" ("style"=:"position:relative;display:inline-block") $ elClass "div" cssClass $ mdo
  -- whitespace <- clickableDivClass'' (constDyn (case iVal of (Layers _ _)->",    ";otherwise->"     ")) "whiteSpaceClickable" ()
  whitespace <- mouseOverClickableDiv (constDyn (case iVal of (Layers _ _)->",    ";otherwise->"     ")) "whiteSpaceClickable" ()
  openCloseEvents <- toggle False $ leftmost [whitespace, closeEvents,(() <$) addEvent]
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (whitespacePopup liveness popupList) False (updated openCloseEvents)
  let addEvent = (ChangeValue (Blank Inert) <$) $ ffilter (\x-> if isJust x then fromJust (fmap (isChangeValue) x) else False) popupMenu
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x == Just MakeL4 || x == Just Eval) popupMenu
  let delContEv = fmap fromJust $ ffilter (\x-> x==Just DeleteContainer) popupMenu
  let layerSplit = fmap fromJust $ ffilter (\x-> x==Just LayerSplit) popupMenu
  let closeEvents = (() <$) $ ffilter (==Nothing) popupMenu
  return $ constDyn ((),leftmost [livenessEv, addEvent,delContEv,layerSplit],never)
  --where
  --  iValSingle (Group (Live (xs,r) _) p) = iValSingle (xs!!0)
  --  iValSingle (Group (Edited _ (xs,r)) p) = iValSingle (xs!!0)
  --  iValSingle (Layers (Live (xs,r) _) p) = iValSingle (xs!!0)
  --  iValSingle (Layers (Edited _ (xs,r)) p) = iValSingle (xs!!0)
  --  iValSingle (Atom x p r) = Atom x p r



whitespacePopup::(MonadWidget t m,Show a)=> Dynamic t Liveness -> [EditSignal a]  -> m (Event t (Maybe (EditSignal a)))
whitespacePopup liveness actionList = elClass "div" "popupMenu" $ do
  let popupList = fmap (\x->clickableDivClass' (show x) "noClass" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  layerSplit <- clickableDivClass' "[ , ]" "noClass" (LayerSplit)
  liveWidget <- livenessCheckboxWidget (liveness)
  closeMenu <- clickableDivClass' "close" "noClass" (Nothing)
  return $ leftmost $ events' ++[closeMenu, fmap Just liveWidget, fmap Just layerSplit]


livenessWidget::(MonadWidget t m) =>  Dynamic t Liveness -> m (Event t (EditSignal a))
livenessWidget liveness = elClass "div" "livenessWidget" $ mdo
  livenessText <- mapDyn (\x->if x==L3 then "L3" else "L4") liveness
  livenessButton <- clickableDivClass'' (livenessText) "livenessText" ()
  eval <- clickableDivClass' "Eval" "L3Eval" Eval
  let livenessChange = attachWith (\d e -> if d==L4 then MakeL3 else MakeL4) (current liveness) livenessButton
  return $ leftmost [livenessChange,eval]

livenessCheckboxWidget::(MonadWidget t m ) => Dynamic t Liveness -> m (Event t (EditSignal a))
livenessCheckboxWidget liveness = elClass "div" "livenessWidget" $ do
  text "Live"
  isLive <- mapDyn (==L4) liveness
  cb <- checkboxView (constDyn empty) isLive
  eval <- clickableDivClass' "Eval" "L3Eval" Eval
  return $ leftmost [fmap (\x-> if x then MakeL4 else MakeL3) cb,eval]

basicPopup::(MonadWidget t m,Show a)=> Dynamic t Liveness -> [EditSignal a]  -> m (Event t (Maybe (EditSignal a)))
basicPopup liveness actionList = elClass "div" "popupMenu" $ do
  let popupList = fmap (\x->clickableDivClass' (show x) "noClass" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  liveWidget <- livenessCheckboxWidget (liveness)
  closeMenu <- clickableDivClass' "close" "noClass" (Nothing)
  return $ leftmost $ events' ++[closeMenu, fmap Just liveWidget]

samplePickerPopup::(MonadWidget t m)=>  Dynamic t Liveness -> Map Int (String,String) -> [EditSignal  String] -> m (Event t (Maybe (EditSignal String)),Event t Hint)
samplePickerPopup liveness sampleMap actionList  = elClass "div" "popupMenu" $ do
  dd <- dropdownOpts (-1) sampleMap def  --defaults to -1 so that someone can select "~" (the first one) and have it register as a change
  let sampleKey = _dropdown_value dd
  sampleChange <- mapDyn (\x-> maybe ("~") (snd) $ Data.Map.lookup x sampleMap) sampleKey -- Dyn (editsignal String)
  let popupList = fmap (\x->clickableDivClass' (show x) "noClass" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  liveWidget <- livenessCheckboxWidget liveness
  closeMenu <- clickableDivClass' "close" "noClass" (Nothing)
  return $ (leftmost $ events' ++[closeMenu, fmap Just liveWidget,fmap (Just . ChangeValue) (updated sampleChange)], fmap SampleHint $ ffilter (\x->if x =="~" then False else True) $ updated sampleChange)

repDivWidget'::MonadWidget t m => RepOrDiv -> Event t () -> m (Event t RepOrDiv)
repDivWidget' iVal _ = elClass "span" "repOrDiv" $ mdo
  repDivButton <- clickableSpanClass showRep "repDivSpan" ()
  repTog <- toggle iToggle repDivButton
  showRep <- mapDyn (\x-> if x then " * " else " / ") repTog
  let textAttrs = constDyn $ fromList $ zip ["min", "class"] ["1","repOrDivInput"]
  textField <- textInput $ def & textInputConfig_attributes .~ textAttrs & textInputConfig_initialValue .~ (show iNum) & textInputConfig_inputType .~"number"
  let numTextField = _textInput_value textField
  num <- mapDyn (\str-> if isJust (readMaybe str::Maybe Int) then (read str::Int) else iNum) numTextField
  dynVal <- combineDyn (\tog val -> if tog then Rep val else Div val) repTog num
  return $ updated dynVal
  where
    (iToggle, iNum) = case iVal of
      (Rep x) -> (True,x)
      (Div x) -> (False,x)
      otherwise -> (True, 1)

repDivWidget''::MonadWidget t m => RepOrDiv -> Event t () -> m (Dynamic t RepOrDiv)
repDivWidget'' iVal _ = elClass "span" "repOrDiv" $ mdo
  repDivButton <- clickableSpanClass showRep "repDivSpan" ()
  repTog <- toggle iToggle repDivButton
  showRep <- mapDyn (\x-> if x then " * " else " / ") repTog
  let textAttrs = constDyn $ fromList $ zip ["min", "class"] ["1","repOrDivInput"]
  textField <- textInput $ def & textInputConfig_attributes .~ textAttrs & textInputConfig_initialValue .~ (show iNum) & textInputConfig_inputType .~"number"
  let numTextField = _textInput_value textField
  num <- mapDyn (\str-> if isJust (readMaybe str::Maybe Int) then (read str::Int) else iNum) numTextField
  combineDyn (\tog val -> if tog then Rep val else Div val) repTog num
  where
    (iToggle, iNum) = case iVal of
      (Rep x) -> (True,x)
      (Div x) -> (False,x)
      otherwise -> (True, 1)


genericSignalMenu :: MonadWidget t m => m (Event t (Maybe (EditSignal a)))
genericSignalMenu = elAttr "div" (singleton "style" "top: 0px; left: 0px; position: absolute; z-index: 1;") $ do
  a <- clickableDivClass' "Close" "noClass" Nothing
  b <- clickableDivClass' "-" "noClass" (Just DeleteMe)
  c <- clickableDivClass' "[]" "noClass" (Just MakeGroup)
  d <- clickableDivClass' "{}" "noClass" (Just MakeLayer)
  return $ leftmost [a,b,c,d]

popupSignalWidget :: MonadWidget t m => m (Event t (EditSignal a))
popupSignalWidget = elAttr "div" (singleton "style" "border: 1px solid black; position: relative; display: inline-block;") $ mdo
  y <- popup popupEvents
  x <- clickableWhiteSpace
  let popupEvents = leftmost [Just genericSignalMenu <$ x,Nothing <$ y]
  return $ (fmap fromJust . ffilter isJust) y



genericSignalWidget :: MonadWidget t m => m (Event t (EditSignal a))
genericSignalWidget = elClass "div" "genericSignalWidget" $ do
  b <- button' "-" DeleteMe
  c <- button' "[]" MakeGroup
  d <- button' "{}" MakeLayer
  return $ leftmost [b,c,d]

hideableWidget :: MonadWidget t m => Dynamic t Bool -> String -> m a -> m a
hideableWidget b c m = do
  attrs <- mapDyn (bool (fromList [("hidden","true"),("class",c)]) (singleton "class" c)) b
  elDynAttr "div" attrs m

hideableWidget' :: MonadWidget t m => Dynamic t Bool -> m a -> m a
hideableWidget' b m = do
  attrs <- mapDyn (bool (fromList [("hidden","true")]) (fromList [("visible","true")])) b
  elDynAttr "div" attrs m
