{-# LANGUAGE RecursiveDo, OverloadedStrings #-}


module Estuary.Widgets.Generic where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Control.Monad
import Control.Monad.IO.Class
import Estuary.Reflex.Utility
import Data.Map
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bool(bool)
import Control.Monad.Fix

import Estuary.Reflex.Utility
import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Reflex.Container
import Data.Maybe
import Text.Read (readMaybe)
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal.Pure as P

import Estuary.Types.Hint


data EditSignal a = ChangeValue a | MakeNew | Close | DeleteMe | RepDiv | MakeGroup | MakeLayer
 | RebuildMe | MakeL3 | MakeL4 | MakeRepOrDiv | Eval | DeleteContainer | LayerSplit | TransformMe deriving (Eq)

instance Functor EditSignal where
  fmap f (ChangeValue x) = ChangeValue $ f x

toPotential::EditSignal a -> Potential a
toPotential (ChangeValue a) = Potential a
toPotential (MakeL3) = PotentialLiveness L3
toPotential (MakeL4) = PotentialLiveness L4
toPotential (Close) = Inert
toPotential (MakeRepOrDiv) = PotentialRepOrDiv
toPotential (MakeGroup) = PotentialMakeGroup
toPotential (MakeLayer) = PotentialMakeLayer
toPotential (DeleteMe) = PotentialDelete

toEditSigGenPat :: EditSignal a -> EditSignal (GeneralPattern a)
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

justChangeValues :: EditSignal a -> Maybe a
justChangeValues (ChangeValue x) = Just x
justChangeValues _ = Nothing

clickableDivDynAttrsWChild :: MonadWidget t m => Dynamic t (Map Text Text) -> m a -> m (Event t ()) -- return (Event t (), a)
clickableDivDynAttrsWChild attrs child = do
  (element,_) <- elDynAttr' "div" attrs $ child
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ (() <$) clickEv

clickableTrWChild :: MonadWidget t m => m a -> m (Event t (), a) -- return (Event t (), a)
clickableTrWChild child = do
  (element, a) <- el' "tr" $ child -- elAttr' "div" (singleton "class" c) $ child -- look elAttr' ::
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  let event = (() <$) clickEv
  return $ (event, a)

clickableDiv :: MonadWidget t m => Text -> m a -> m (Event t ())
clickableDiv cssclass child = do
  (element,_) <- elAttr' "div" attr $ child
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ (() <$) clickEv
  where
    attr = singleton "class" cssclass

clickableDivClass :: MonadWidget t m => Text -> Text -> m (Event t ())
clickableDivClass label c = do
  (element,_) <- elAttr' "div" (singleton "class" c) $ text label
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ (() <$) clickEv

clickableDivClass' :: MonadWidget t m => Text -> Text -> a -> m (Event t a)
clickableDivClass' label c e = liftM (e <$) $ clickableDivClass label c

-- with displayed text that can change
clickableDivClass'':: MonadWidget t m => Dynamic t Text -> Text -> a -> m (Event t a)
clickableDivClass'' label c e = do
  (element, _) <- elAttr' "div" ("class"=:c) $ dynText label
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ (e <$) clickEv

mouseOverClickableDiv::MonadWidget t m => Dynamic t Text -> Text -> a -> m(Event t a)
mouseOverClickableDiv label c e = mdo
  (element, _) <- elDynAttr' "div" attrs $ dynText label
  mouseOver <- liftM (True <$) $ wrapDomEvent (_el_element element) (elementOnEventName Mouseover) mouseXY
  mouseOut <- liftM (False <$) $ wrapDomEvent (_el_element element) (elementOnEventName Mouseout) mouseXY
  isMouseOver <- holdDyn False $ leftmost [mouseOut, mouseOver]
  let attrs  = fmap (fromList . (\x-> [("class",c),x]) . ((,) "style") . bool "" ";background-color:rgba(144,238,144,0.2);") isMouseOver
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ (e <$) clickEv


clickableDivAttrs::MonadWidget t m => Text -> a -> Map Text Text -> m (Event t a)
clickableDivAttrs label val attrs= do
  (element,_) <- elAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ (val <$) clickEv

clickableDivAttrs'::MonadWidget t m => Text -> a -> Map Text Text -> x -> y -> m (Dynamic t ((),Event t a))
clickableDivAttrs' label val attrs _ _= do
  (element,_) <- elAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  let event = (val <$) clickEv
  return $ constDyn ((),event)

clickableDivDynAttrs :: MonadWidget t m => Text -> a -> Dynamic t (Map Text Text) -> m (Event t a)
clickableDivDynAttrs label val attrs = do
  (element,_) <- elDynAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ (val <$) clickEv

-- with displayed text that can change
clickableSpanClass:: MonadWidget t m => Dynamic t Text -> Text -> a -> m (Event t a)
clickableSpanClass label c e = do
  (element, _) <- elAttr' "span" ("class"=:c) $ dynText label
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ (e <$) clickEv

clickableTdClass::MonadWidget t m => Dynamic t Text -> Dynamic t Text -> a -> m (Event t a)
clickableTdClass label c val = do
  let attrs = fmap (singleton "class") c
  (element, _) <- elDynAttr' "td" attrs $ dynText label
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ ((val) <$) clickEv


pingButton :: MonadWidget t m => Text -> m (Event t ())
pingButton label = liftM (() <$) $ button label

pingButton' :: MonadWidget t m => Text -> m (Dynamic t ((),Event t ()))
pingButton' label = do
  x <- pingButton label
  return $ constDyn ((),x)

pingButton'' :: MonadWidget t m => Text -> a -> b -> m (Dynamic t ((),Event t ()))
pingButton'' label _ _ = pingButton' label

pingButton''':: MonadWidget t m => Text -> Map Text Text -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())))
pingButton''' label attrs _ _ = do
  b <- buttonDynAttrs label (ChangeValue ()) $ constDyn attrs
  return $ constDyn ((), b)

makeNewButton:: (MonadWidget t m)=> Text -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())) )
makeNewButton label _ _ = do
  a <- button label
  return $ constDyn ((), ((MakeNew::EditSignal ()) <$) a)


tdButtonAttrs:: MonadWidget t m => Text -> a -> Map Text Text -> m (Event t a)
tdButtonAttrs s val attrs = do
  (element, _) <- elAttr' "td" attrs $ text s
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ ((val) <$) clickEv

-- with displayed text that can change
tdButtonAttrs':: MonadWidget t m => Dynamic t Text -> a -> Map Text Text -> m (Event t a)
tdButtonAttrs' s val attrs = do
  (element, _) <- elAttr' "td" attrs $ dynText s
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ ((val) <$) clickEv

tdPingButtonAttrs:: MonadWidget t m => Text -> Map Text Text -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())))
tdPingButtonAttrs label attrs _ _ = el "td" $ do
  b <- buttonDynAttrs label (ChangeValue ()) $ constDyn attrs
  return $ constDyn ((), b)

growingTextInput::MonadWidget t m => TextInputConfig t -> m (TextInput t)
growingTextInput config = mdo
  let attrs = _textInputConfig_attributes config
  let dynAttrs = (\m w-> insertWith (T.append) "style" (T.pack $ ";width:"++ show (max 20 $ min 100 $ 8*T.length w) ++ "px" ++";") m) <$> attrs <*> (_textInput_value textField)
  let newConfig = TextInputConfig (_textInputConfig_inputType config) (_textInputConfig_initialValue config) (_textInputConfig_setValue config) dynAttrs
  textField <- textInput newConfig
  return textField

whitespace:: (MonadWidget t m, Show a, Eq a)=> Dynamic t Liveness -> GeneralPattern a -> Text -> [EditSignal (GeneralPattern a)] -> () -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t ((), Event t (EditSignal (GeneralPattern a)), Event t Hint))
whitespace liveness iVal cssClass popupList _ event = elAttr "div" ("style"=:"position:relative;display:inline-block") $ elClass "div" cssClass $ mdo
  -- whitespace <- clickableDivClass'' (constDyn (case iVal of (Layers _ _)->",    ";otherwise->"     ")) "whiteSpaceClickable" ()
  whitespace <- mouseOverClickableDiv (constDyn (case iVal of (Layers _ _)->",    ";otherwise->"     ")) "whiteSpaceAdd" ()
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
  let popupList = fmap (\x->clickableDivClass' (T.pack $ show x) "primary-color code-font background" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  layerSplit <- clickableDivClass' "[ , ]" "primary-color code-font background" (LayerSplit)
  liveWidget <- livenessCheckboxWidget (liveness)
  closeMenu <- clickableDivClass' "close" "primary-color code-font background" (Nothing)
  return $ leftmost $ events' ++[closeMenu, fmap Just liveWidget, fmap Just layerSplit]


livenessWidget::(MonadWidget t m) =>  Dynamic t Liveness -> m (Event t (EditSignal a))
livenessWidget liveness = elClass "div" "livenessWidget" $ mdo
  let livenessText = fmap (\x->if x==L3 then "L3" else "L4") liveness
  livenessButton <- clickableDivClass'' (livenessText) "livenessText" ()
  eval <- clickableDivClass' "Eval" "L3Eval" Eval
  let livenessChange = attachWith (\d e -> if d==L4 then MakeL3 else MakeL4) (current liveness) livenessButton
  return $ leftmost [livenessChange,eval]

livenessCheckboxWidget::(MonadWidget t m ) => Dynamic t Liveness -> m (Event t (EditSignal a))
livenessCheckboxWidget liveness = elClass "div" "livenessWidget" $ do
  text "Live"
  let isLive = fmap (==L4) liveness
  cb <- checkboxView (constDyn empty) isLive
  eval <- clickableDivClass' "Eval" "L3Eval" Eval
  return $ leftmost [fmap (\x-> if x then MakeL4 else MakeL3) cb,eval]

basicPopup::(MonadWidget t m,Show a)=> Dynamic t Liveness -> [EditSignal a]  -> m (Event t (Maybe (EditSignal a)))
basicPopup liveness actionList = elClass "div" "popupMenu" $ do
  let popupList = fmap (\x->clickableDivClass' (T.pack $ show x) "primary-color code-font background" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  liveWidget <- livenessCheckboxWidget (liveness)
  closeMenu <- clickableDivClass' "close" "primary-color code-font background" (Nothing)
  return $ leftmost $ events' ++[closeMenu, fmap Just liveWidget]

samplePickerPopup::(MonadWidget t m)=>  Dynamic t Liveness -> Map Int (Text,Text) -> [EditSignal  Text] -> m (Event t (Maybe (EditSignal Text)),Event t Hint)
samplePickerPopup liveness sampleMap actionList  = elClass "div" "popupMenu" $ do
  dd <- dropdownOpts (-1) sampleMap def  --defaults to -1 so that someone can select "~" (the first one) and have it register as a change
  let sampleKey = _dropdown_value dd
  let sampleChange = fmap (\x-> maybe ("~") (snd) $ Data.Map.lookup x sampleMap) sampleKey -- Dyn (editsignal Text)
  let popupList = fmap (\x->clickableDivClass' (T.pack $ show x) "primary-color code-font background" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  liveWidget <- livenessCheckboxWidget liveness
  closeMenu <- clickableDivClass' "close" "primary-color code-font background" (Nothing)
  return $ (leftmost $ events' ++[closeMenu, fmap Just liveWidget,fmap (Just . ChangeValue) (updated sampleChange)], fmap SampleHint $ ffilter (\x->if x =="~" then False else True) $ updated sampleChange)

repDivWidget'::MonadWidget t m => RepOrDiv -> Event t () -> m (Event t RepOrDiv)
repDivWidget' iVal _ = elClass "span" "repOrDiv" $ mdo
  repDivButton <- clickableSpanClass showRep "repDivSpan" ()
  repTog <- toggle iToggle repDivButton
  let showRep = fmap (\x-> if x then " * " else " / ") repTog
  let textAttrs = constDyn $ fromList $ zip ["min", "class"] ["1","repOrDivInput"]
  textField <- textInput $ def & textInputConfig_attributes .~ textAttrs & textInputConfig_initialValue .~ (T.pack $ show iNum) & textInputConfig_inputType .~"number"
  let numTextField = _textInput_value textField
  let num = fmap (\str-> if isJust (readMaybe (T.unpack str)::Maybe Int) then (read (T.unpack str)::Int) else iNum) numTextField
  let dynVal = (\tog val -> if tog then Rep val else Div val) <$> repTog <*> num
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
  let showRep = fmap (\x-> if x then " * " else " / ") repTog
  let textAttrs = constDyn $ fromList $ zip ["min", "class"] ["1","repOrDivInput"]
  textField <- textInput $ def & textInputConfig_attributes .~ textAttrs & textInputConfig_initialValue .~ (T.pack $ show iNum) & textInputConfig_inputType .~"number"
  let numTextField = _textInput_value textField
  let num = fmap (\str-> if isJust (readMaybe (T.unpack str)::Maybe Int) then (read (T.unpack str)::Int) else iNum) numTextField
  return $ (\tog val -> if tog then Rep val else Div val) <$> repTog <*> num
  where
    (iToggle, iNum) = case iVal of
      (Rep x) -> (True,x)
      (Div x) -> (False,x)
      otherwise -> (True, 1)


genericSignalMenu :: MonadWidget t m => m (Event t (Maybe (EditSignal a)))
genericSignalMenu = elAttr "div" (singleton "style" "top: 0px; left: 0px; position: absolute; z-index: 1;") $ do
  a <- clickableDivClass' "Close" "primary-color code-font background" Nothing
  b <- clickableDivClass' "-" "primary-color code-font background" (Just DeleteMe)
  c <- clickableDivClass' "[]" "primary-color code-font background" (Just MakeGroup)
  d <- clickableDivClass' "{}" "primary-color code-font background" (Just MakeLayer)
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

-- Used heavily in Help section
-- Contains the child in a hideable div with a class
hideableWidget :: MonadWidget t m => Dynamic t Bool -> Text -> m a -> m a
hideableWidget b c m = do
  let attrs = fmap (bool (fromList [("hidden","true"),("class",c)]) (singleton "class" c)) b
  elDynAttr "div" attrs m

-- Used for the terminal
-- Contains the child in a hideable div with no class
hideableWidget' :: MonadWidget t m => Dynamic t Bool -> m a -> m a
hideableWidget' b m = do
  let attrs = fmap (bool (fromList [("hidden","true")]) (fromList [("visible","true")])) b
  elDynAttr "div" attrs m

hideableWidgetWFlexColumn :: MonadWidget t m => Dynamic t Bool -> m a -> m a
hideableWidgetWFlexColumn b  m = do
  let attrs = fmap (bool (fromList [("hidden","true")]) (fromList [("style", "display: flex; flex-direction: column;")])) b
  elDynAttr "div" attrs m

traceDynamic :: (MonadWidget t m, Show a) => String -> Dynamic t a -> m (Dynamic t a)
traceDynamic m x = do
  initialValue <- sample $ current x
  let x' = traceEvent m $ updated x
  holdDyn initialValue x'

-- a hideable widget that is only built/rebuilt when it is made visible
deferredWidget :: (MonadFix m, DomBuilder t m, MonadSample t m, MonadHold t m, Adjustable t m, NotReady t m, PostBuild t m) => Text -> Dynamic t Bool -> Dynamic t (m ()) -> m ()
deferredWidget cssClass isVisible dynWidgets = do
  initialVisibility <- sample $ current isVisible
  defaultWidget <- sample $ current dynWidgets
  let initialWidget = if initialVisibility then defaultWidget else return ()
  isVisible' <- holdUniqDyn isVisible
  let transitionsToVisible = ffilter (== True) $ updated isVisible'
  let becomesVisible = tag (current dynWidgets) transitionsToVisible
  let visibleChanges = gate (current isVisible) $ updated dynWidgets
  let changes = leftmost [becomesVisible,visibleChanges]
  let attrs = fmap (bool (fromList [("hidden","true"),("class",cssClass)]) (singleton "class" cssClass)) isVisible
  elDynAttr "div" attrs $ widgetHold initialWidget changes
  return ()

--this is a special type of tooltip that is used in statusWidget, tooltip has position relative
tooltipForScrollableTable :: DomBuilder t m => m a -> m b -> m a
tooltipForScrollableTable child popup = do
  divClass "tooltip-scrollable-table" $ do
    a <- child
    elClass "span" "tooltiptext-scrollable-table" popup
    return a

-- this is a standard tooltip that "overrides" the overflow hidden of its parents.
--this won't work fine if the label is inside a scrollable div.
tooltip :: DomBuilder t m => m a -> m b -> m a
tooltip child popup = do
  elClass "div" "tooltip" $ do
    a <- child
    divClass "tooltipPosAbsolute" $ elClass "span" "tooltiptext code-font" popup
    return a
