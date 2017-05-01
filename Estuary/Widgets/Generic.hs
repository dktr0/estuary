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

import Estuary.Reflex.Container 
import Data.Maybe
import Text.Read (readMaybe)
import Estuary.WebDirt.Foreign
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal.Pure as P


data Hint = SampleHint String deriving (Eq)

data EditSignal a = ChangeValue a | MakeNew | Close | DeleteMe | RepDiv | MakeGroup | MakeLayer
 | RebuildMe | MakeL3 | MakeL4 | MakeRepOrDiv | Eval | DeleteContainer | LayerSplit  deriving (Eq)

toPotential::EditSignal a -> Potential a
toPotential (ChangeValue a) = Potential a
toPotential (MakeL3) = PotentialLiveness L3
toPotential (MakeL4) = PotentialLiveness L4
toPotential (Close) = Inert



-- Edit signal used right now to signal certain events up... are those events always potentials?.... no...
-- but the Potentials [] should be derrived somehow from the possible editsignals that the popup menu will be able to 
-- send up.


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

doHint :: T.JSVal -> Hint -> IO ()
doHint wd (SampleHint x) = sampleHint wd (P.pToJSVal x)

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


--whitespacePopup:: (MonadWidget t m, Show a, Eq a)=> Dynamic t Context -> GeneralPattern a -> String -> [EditSignal (GeneralPattern a)] -> () -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t ((), Event t (EditSignal (GeneralPattern a) )))
--whitespacePopup liveness iVal cssClass popupList _ event = elClass "div" cssClass $ mdo
--  whitespace <- clickableDivClass'' (constDyn "     ") "whiteSpaceClickable" ()
--  openCloseEvents <- toggle False $ leftmost [whitespace, closeEvents,(() <$) addEvent]
--  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (basicPopup liveness popupList) False (updated openCloseEvents)
--  let addEvent = (ChangeValue (iVal) <$) $ ffilter (\x-> if isJust x then fromJust (fmap (isChangeValue) x) else False) popupMenu
--  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x == Just MakeL4 || x == Just Eval) popupMenu
--  let delContEv = fmap fromJust $ ffilter (\x-> x==Just DeleteContainer) popupMenu
--  let closeEvents = (() <$) $ ffilter (==Nothing) popupMenu
--  return $ constDyn ((),leftmost [livenessEv, addEvent,delContEv])

whitespace:: (MonadWidget t m, Show a, Eq a)=> Dynamic t Liveness -> GeneralPattern a -> String -> [EditSignal (GeneralPattern a)] -> () -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t ((), Event t (EditSignal (GeneralPattern a) )))
whitespace liveness iVal cssClass popupList _ event = elClass "div" cssClass $ mdo
  whitespace <- clickableDivClass'' (constDyn "     ") "whiteSpaceClickable" ()
  openCloseEvents <- toggle False $ leftmost [whitespace, closeEvents,(() <$) addEvent]
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (whitespacePopup liveness popupList) False (updated openCloseEvents)
  let addEvent = (ChangeValue (iValSingle iVal) <$) $ ffilter (\x-> if isJust x then fromJust (fmap (isChangeValue) x) else False) popupMenu
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x == Just MakeL4 || x == Just Eval) popupMenu
  let delContEv = fmap fromJust $ ffilter (\x-> x==Just DeleteContainer) popupMenu
  let layerSplit = fmap fromJust $ ffilter (\x-> x==Just LayerSplit) popupMenu
  let closeEvents = (() <$) $ ffilter (==Nothing) popupMenu
  return $ constDyn ((),leftmost [livenessEv, addEvent,delContEv,layerSplit])
  where
    iValSingle (Group (Live (xs,r) _) p) = iValSingle (xs!!0)
    iValSingle (Group (Edited _ (xs,r)) p) = iValSingle (xs!!0)
    iValSingle (Layers (Live (xs,r) _) p) = iValSingle (xs!!0)
    iValSingle (Layers (Edited _ (xs,r)) p) = iValSingle (xs!!0)
    iValSingle (Atom x p r) = Atom x p r



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

samplePickerPopup::(MonadWidget t m)=>  Dynamic t Liveness -> Map Int (String,String) -> [EditSignal (GeneralPattern String)] -> m (Event t (Maybe (EditSignal (GeneralPattern String))))
samplePickerPopup liveness sampleMap actionList  = elClass "div" "popupMenu" $ do
  dd <- dropdownOpts 0 sampleMap def 
  let sampleKey = _dropdown_value dd 
  -- @ toPotential is undefined for some values of 'editsignal' - this may break depending on how we're using this widget
  let potential = Potentials $ fmap toPotential actionList
  sampleChange <- mapDyn (\x-> Just $ ChangeValue $ Atom (maybe ("~") (snd) $ Data.Map.lookup x sampleMap) (potential) Once) sampleKey -- Dyn (editsignal String)
  let popupList = fmap (\x->clickableDivClass' (show x) "noClass" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  liveWidget <- livenessCheckboxWidget liveness
  closeMenu <- clickableDivClass' "close" "noClass" (Nothing)
  return $ leftmost $ events' ++[closeMenu, fmap Just liveWidget, (updated sampleChange)]

repDivWidget'::MonadWidget t m => RepOrDiv -> Event t () -> m (Event t RepOrDiv)
repDivWidget' iVal _ = elAttr "div" ("class"=:"repOrDiv") $ mdo
  repDivButton <- clickableDivClass'' showRep "noClass" ()
  repTog <- toggle iToggle repDivButton
  showRep <- mapDyn (\x-> if x then "*" else "/") repTog
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
  --a <- button' "Ping" Ping
  b <- button' "-" DeleteMe
  c <- button' "[]" MakeGroup
  d <- button' "{}" MakeLayer
  return $ leftmost [b,c,d]


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
