{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.GeneralPattern
where

import Reflex
import Reflex.Dom
import Estuary.Reflex.Container
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Estuary.Widgets.Generic
import Control.Monad
import Data.List(intersperse, findIndex, elem)
import GHCJS.DOM.EventM
import Data.Maybe(isJust,listToMaybe,fromMaybe,fromJust)
import Text.Read(readMaybe)
import Control.Applicative (liftA2)


------------------------------------------------
--                GENERAL CONTAINER           --
------------------------------------------------
--generalContainer :: (MonadWidget t m, Eq a) => (GeneralPattern a -> Event t () -> m (Dynamic t (GeneralPattern a, Event t (EditSignal a)))) -> GeneralPattern a -> Event t () -> m (Dynamic t (GeneralPattern a, Event t (EditSignal a)))
--generalContainer b i _ = elClass "div" (getClass i) $ mdo
--  let cEvents = mergeWith (union) [insertMap,deleteMap]
--  (values,events) <- eitherContainer' (initialMap i) cEvents never never leftBuilder (rightBuilder i)
--  let deleteMap = fmap (fromList . concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)]) . keys . Data.Map.filter (==DeleteMe)) events
--  let insertMap = fmap (fromList . concat . (insertList i) . keys . Data.Map.filter (==ChangeValue () ))  events
--  mapDyn (returnF i) values
--  where
--    getClass (Layers _ _) = "generalPattern-layer"
--    getClass (Group _ _) = "generalPattern-group"
--    getClass (Atom _ _) = "generalPattern-atom"
--    initialMap (Layers xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
--    initialMap (Group xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
--    initialMap (Atom iVal iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
--    leftBuilder = aGLWidget b
--    rightBuilder (Layers _ _) = tdPingButtonAttrs "+" ("class"=:"addButton-vertical")
--    rightBuilder (Group _ _) = pingButton''' "+" ("class"=:"addButton")
--    rightBuilder (Atom _ _) = pingButton''' "+" ("class"=:"addButton")
--    insertList (Atom iVal _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (iVal) Once))])
--    insertList (Layers xs iReps) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
--    insertList (Group xs iReps) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
--    returnF (Layers _ _) x = (Layers (elems x) Once,never)
--    returnF (Group _ _) x = (Group (elems x) Once,never)
--    returnF (Atom _ _) x = (Group (elems x) Once,never)


-- Using clickable whitespace instead of plus buttons
generalContainer :: (MonadWidget t m, Eq a, Show a) => (GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)))))-> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))
generalContainer b i _ = elClass "div" (getClass i) $ mdo
  let cEvents = mergeWith (union) [insertMap,deleteMap]
  --(values,events) <- eitherContainer' (initialMap i) cEvents never never leftBuilder (rightBuilder)
  (values,events) <- eitherContainer' (fromList $ zip [0::Int] [Right ()]) cEvents livenessEv never leftBuilder (rightBuilder)
  -- let livenessEv = fmap (   . Data.Map.filter (\x-> case x of MakeL3 -> " L3"; otherwise -> " L4") )
  let events' = fmap (Data.Map.elems) events -- Event [l]
  let livenessEv = fmap (\x-> if Data.List.elem MakeL3 x then MakeL3 else MakeL4) $ ffilter (\x-> Data.List.elem MakeL3 x || Data.List.elem MakeL4 x) events' -- If any child reports a change 
  dynClass <- holdDyn ("MakeL4") $ fmap (\x-> if x == MakeL3 then "MakeL3" else "MakeL4") livenessEv
  dynText dynClass
  let deleteMap = fmap (fromList . concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)]) . keys . Data.Map.filter (==DeleteMe)) events
  let insertMap = fmap (fromList . concat . (insertList i) . keys . Data.Map.filter (isChangeValue) )  events
  mapDyn (returnF i) values
  where
    initialVal (Atom iV _) = iV
    initialVal (Group iV _) = initialVal $ iV!!0
    initialVal (Layers iV _) = initialVal $ iV!!0
    getClass (Layers _ _) = "generalPattern-layer"
    getClass (Group _ _) = "generalPattern-group"
    getClass (Atom _ _) = "generalPattern-atom"
    initialMap (Layers xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Group xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Atom iVal iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
    leftBuilder = aGLWidget b
    rightBuilder = whitespacePopup (i) "whiteSpaceAdd" [ChangeValue (i)]
    insertList (Atom iVal _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (iVal) Once))])
    insertList (Layers xs iReps) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Group xs iReps) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    returnF (Layers _ _) x = (Layers (elems x) Once,never)
    returnF (Group _ _) x = (Group (elems x) Once,never)
    returnF (Atom _ _) x = (Group (elems x) Once,never)

--tdPingButtonAttrs:: MonadWidget t m => String -> Map String String -> a -> b -> m (Dynamic t ((),Event t (EditSignal a)))

--(EditSignal a)Menu' :: (MonadWidget t m, Eq a )=> Map a String -> m (Event t (Maybe a))


whitespacePopup:: (MonadWidget t m, Show a, Eq a)=> GeneralPattern a -> String -> [EditSignal (GeneralPattern a)] -> () -> b -> m (Dynamic t ((), Event t (EditSignal (GeneralPattern a) )))
whitespacePopup iVal cssClass popupList _ _ = elClass "div" cssClass $ mdo
  whitespace <- clickableDivClass'' (constDyn "     ") "whiteSpaceClickable" ()
  openCloseEvents <- toggle False $ leftmost [whitespace, closeEvents,(() <$) addEvent]
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (genericSignalMenu' popupList) False (updated openCloseEvents)
  let addEvent = (ChangeValue (iVal) <$) $ ffilter (\x-> if isJust x then fromJust (fmap (isChangeValue) x) else False) popupMenu
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x == Just MakeL4) popupMenu
  let closeEvents = (() <$) $ ffilter (==Nothing) popupMenu
  return $ constDyn ((),leftmost [livenessEv, addEvent])

aGLWidget::(MonadWidget t m, Eq a, Show a) => (GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))) -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))
aGLWidget builder iVal ev = mdo
--resettableWidget :: (MonadWidget t m, Eq a)=> (a -> Event t () -> m (Dynamic t (a,Event t (EditSignal a)))) -> a -> Event t () -> Event t a -> m (Dynamic t (a,Event t (EditSignal a)))
  val <- resettableWidget (function) iVal ev rebuildEvent'
  widgetEvents <- forDyn val (\(x,y)->y)
  rebuildEvent <- forDyn widgetEvents (\x-> ffilter (==RebuildMe) x)
  let rebuildEvent' = attachDynWith (\(value,_) _ ->value) val $ switchPromptlyDyn rebuildEvent
  return val
  where
    function (Atom x r) e = builder (Atom x r) e
    function (Blank) e = builder (Blank) e
    function (Group xs r) e = generalContainer builder (Group xs r) e
    function (Layers xs r) e = generalContainer builder (Layers xs r) e




livenessWidget::(MonadWidget t m) => (EditSignal a) -> Event t (EditSignal a) -> m (Event t (EditSignal a))
livenessWidget iLiveness updateEv = elAttr "div" ("class"=:"livenessWidget") $ mdo
  let iIsL3 = case iLiveness of MakeL3 -> True; otherwise ->False
  livenessButton <- clickableDivClass'' (livenessText) "livenessText" ()
  let livenessTextEv = fmap (\x-> if isMake4 x then "L4" else "L3") updateEv  -- used where binding instead of if to avoid needing Eq
  livenessText <- holdDyn (if iIsL3 then "L3" else "L4") livenessTextEv
  evalButton <- liftM switchPromptlyDyn $ flippableWidget (return never) (clickableDivClass' "Eval" "L3Eval" Eval) iIsL3 (fmap isMake4 updateEv)
  -- @Temp 
  liveness <- toggle (iIsL3) livenessButton -- Dyn bool

  return $ leftmost $ [evalButton]++[fmap (\x-> if x then MakeL3 else MakeL4) $ updated liveness]
  where 
    isMake4 (MakeL4) = True
    isMake4 (MakeL3) = False


-- @ would it make more sense as:
-- => Map GenericSignal String -> m (Event t (Maybe GenericSignal))?
genericSignalMenu'::(MonadWidget t m,Show a, Eq a)=> [EditSignal a] -> m (Event t (Maybe (EditSignal a)))
genericSignalMenu' actionList = elClass "div" "popupMenu" $ do
  --let popUpMap = mapWithKey (\k v-> clickableDivClass' v "noClass" (Just k)) actionMap-- Map k (m Event t (Maybe k))
  let popupList = fmap (\x->clickableDivClass' (show x) "noClass" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  --events <- liftM (Data.Map.elems) widgets
  events' <- liftM (id) events
  liveness <- livenessWidget (MakeL4) never
  let tex = fmap (\x->case x of MakeL4 -> "L4"; otherwise -> "L3") liveness
  holdDyn "yoink" tex >>= dynText
  closeMenu <- clickableDivClass' "close" "noClass" (Nothing)
  return $ leftmost $ events' ++[closeMenu, fmap Just liveness]


-- @ clean up redundant/ugly code...
popupSampleWidget :: MonadWidget t m => GeneralPattern String -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern String, Event t (EditSignal (GeneralPattern String))))
popupSampleWidget iVal e = elAttr "div" (singleton "style" "border: 1px solid black; position: relative; display: inline-block;") $ mdo
  let (iSamp,iRepDiv) = case iVal of 
                    (Group xs r) -> (show $ xs!!0,r) 
                    (Layers xs r) -> (show $ xs!!0,r) 
                    (Atom v r) -> (v,r)
                    otherwise -> ("~",Once)
  let divPopupIsViewable = and $ fmap (/=iRepDiv) [Rep 1, Div 1, Once]  -- Only show the rep/div popup if initial rep Div is not one of these
  --let popupMap = fromList $ zip [1::Int,2..] ["bd","sn", "cp","[]", "[,,]","* Or /","Delete"] 
  let popupMap = [ChangeValue (Atom "bd" Once), ChangeValue (Atom "sn" Once), MakeRepOrDiv, MakeGroup, MakeLayer, DeleteMe]
  x <- clickableDivClass'' sampText "noClass" ()

  repDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) divPopupIsViewable $ updated repDivToggle

  y <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (genericSignalMenu' popupMap) False (updated popupEvents')

  

  --let closeEvents = (() <$)  $ ffilter (==Nothing)  y
  --let groupEv = (MakeGroup <$) $ ffilter (==Just 4) y
  --let layerEv = (MakeLayer <$) $ ffilter (==Just 5) y
  --let deleteEv = (DeleteMe <$) $ ffilter (==Just 7) y
  ----let livenessEv = fmap ( ) $ ffilter (==Just 8) y
  --let groupEv = fmap (MakeGroup::(EditSignal (GeneralPattern String))  <$) . fromJust) $ ffilter (==Just MakeGroup) y
  --let layerEv = fmap (MakeLayer::(EditSignal (GeneralPattern String))  <$) . fromJust) $ ffilter (==Just MakeLayer) y
  --let deleteEv = fmap ( MakeGroup::(EditSignal (GeneralPattern String))  <$) . fromJust) $ ffilter (==Just DeleteMe) y
  let closeEvents = (() <$)  $ ffilter (==Nothing)  y
  let groupEv = fmap fromJust $ ffilter (==Just MakeGroup) y
  let layerEv = fmap fromJust  $ ffilter (==Just MakeLayer) y
  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) y
  repDivToggle <- toggle divPopupIsViewable $ ffilter (== Just MakeRepOrDiv) y
  let sampleChanges = ffilter (\x-> if isJust x then isChangeValue $ fromJust x else False) y -- Event t (Maybe (EditSignal a))
  let sampleChanges' = fmap (\x-> maybe Blank (\y->case y of (ChangeValue z)-> z; otherwise -> Blank) x) sampleChanges
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4) y
  --let livenessEv = fmap ( ) $ ffilter (==Just 8) y

  groupToggle <- toggle False groupEv
  layerToggle <- toggle False layerEv
  --let sampleChanges = fmap (fromMaybe 0) $ ffilter (\x-> if Data.Maybe.isJust x then (x>=Just 1 && x<=Just 3) else False)  y
  --let sampleChanges' = fmap (\k-> maybe iVal (\x-> Atom x Once) $ Data.Map.lookup k popupMap ) sampleChanges
  sampText <- holdDyn iSamp $ fmap show sampleChanges'
  popupEvents' <- toggle False $ leftmost $ [x, closeEvents] ++ [(() <$)  y]

  repDivVal <- holdDyn iRepDiv repDivEv >>= combineDyn (\tog val -> if tog then val else Once) repDivToggle
  genPat <- combineDyn (\x y -> Atom x y) sampText repDivVal

  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  --mapDyn (\val-> (val, never)) genPat''

  let signalEvents = leftmost $ [deleteEv,livenessEv] ++ [leftmost $ fmap (RebuildMe <$)[groupEv, layerEv]]
  mapDyn (\val-> (val, signalEvents)) genPat''


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


------------------------
----  GenPat Double   --
------------------------
-- A groupable/layerable/atomizable widget for General Pattern Doubles
-- vMin and vMax denote the rane of possible values, step = the stepsize of each increment


aGLDoubleWidget::(MonadWidget t m) => Double -> Double -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
aGLDoubleWidget vMin vMax step (Atom iVal _) _ = elAttr "table" ("class"=:"aGLNumberWidgetTable") $ mdo
  (genPat,deleteEvent) <- el "tr" $ do
    genPat <- elAttr "td" ("class"=:"aGLNumberWidgetTable-textFieldtd") $ do
      let attrs = fromList $ zip ["style","step","min","max"] ["width=40px",show step, show vMin, show vMax]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      inVal' <- mapDyn (\str-> if isJust (readMaybe str::Maybe Double) then (read str::Double) else (vMax-vMin)/2+vMin) inVal
      mapDyn (\x->Atom (max vMin $ min vMax x) Once) inVal'
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ ("class"=:"aGLNumberWidgetTable-deletetd")
    return (genPat, deleteButton)
  (layerEvent, groupEvent) <- el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLNumberWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLNumberWidgetTable-layertd")
    return $ (layerButton, groupButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x->(x,leftmost [rebuildEvent, deleteEvent])) genPat''
aGLDoubleWidget vMin vMax step _ e = aGLDoubleWidget vMin vMax step (Atom 0 Once) e

----------------------
--  GenPat Ints     --
----------------------
-- A groupable/layerable/atomizable widget for General Pattern Ints
-- vMin and vMax denote the rane of possible values, step = the stepsize of each increment

aGLIntWidget::(MonadWidget t m) => Int -> Int -> Int -> GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t (EditSignal a)))
aGLIntWidget vMin vMax step (Atom iVal _) _ = elAttr "table" ("class"=:"aGLNumberWidgetTable") $ mdo
  (genPat,deleteEvent) <- elAttr "tr" (empty) $ do
    genPat <- elAttr "td" ("class"=:"aGLNumberWidgetTable-textFieldtd") $ do
      let attrs = fromList $ zip ["style","step","min","max"] ["width:40px",show step, show vMin, show vMax]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      inVal' <- mapDyn (\str-> if isJust (readMaybe str::Maybe Int) then (read str::Int) else 0) inVal
      mapDyn (\x->Atom (max vMin $ min vMax x) Once) inVal'
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ "class"=:"aGLNumberWidgetTable-deletetd"
    return (genPat, deleteButton)
  (layerEvent, groupEvent) <- el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLNumberWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLNumberWidgetTable-layertd")
    return $ (layerButton, groupButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x->(x,leftmost [rebuildEvent, deleteEvent])) genPat''
aGLIntWidget vMin vMax step _ e = aGLIntWidget vMin vMax step (Atom 0 Once) e



----------------------
--  GenPat Strings  --
----------------------
-- The following 3 widget functions compose of a container which interpserses aGLStringWidgets
-- (something that returns an Atom, Group or Layer (aGL)) with + buttons. The widgets can be
-- recursively constructed: each individual widget can be turned into a container itself (as a group or layer).

-- Individual string widgets (able to turn into a container themselves by signaling their container in the returned event)
aGLStringWidget::(MonadWidget t m) => GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t (EditSignal a)))
--aGLStringWidget (Atom iVal iReps) _ = elAttr "div" ("style"=:"display:inline-block;") $ elAttr "table" tableAttrs $ mdo
aGLStringWidget (Atom iVal iReps) _ = elAttr "table" ("class"=:"aGLStringWidgetTable") $ mdo
  genPat <- el "tr" $ do
    val <- elAttr "td" ("class"=:"aGLStringWidgetTable-textFieldtd") $ do
      inputField <- textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:60px") & textInputConfig_initialValue .~ (iVal)
      let val = _textInput_value inputField
      return val
    reps <- repDivWidget iReps
    combineDyn (\x y -> Atom x y) val reps
  (layerEvent, groupEvent, deleteEvent) <- el "tr" $ elAttr "td" ("colspan"=:"3") $ el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLNumberWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLNumberWidgetTable-layertd")
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ "class"=:"aGLNumberWidgetTable-deletetd"
    return $ (layerButton, groupButton, deleteButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = (RebuildMe <$) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x-> (x,leftmost [rebuildEvent, deleteEvent])) genPat''
aGLStringWidget _ e = aGLStringWidget (Atom "~" Once) e



------------------------------------------
--             UTILITY GENPAT'S         --
------------------------------------------

--Slider w/ a range and stepsize
sliderWidget::MonadWidget t m => (Double,Double)-> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
sliderWidget (minVal,maxVal) stepsize iVal _ = do
  text $ show minVal
  let attrs = constDyn $ fromList $ zip ["type","min","max","step","style"] ["range",show minVal,show maxVal,show stepsize,"width:75px"]
  slider<-textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs
  text $ show maxVal
  let panVal = _textInput_value slider
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Double) then Atom (read x::Double) Once else Atom 0.5 Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton))


-- A clickable td element. Each click cycles to the next element in the map. Updated with a RepOrDiv event.
-- rep/div values get shown on the button too.
clickListWidget::(MonadWidget t m, Show a, Eq a) => Map Int a ->  GeneralPattern a -> Event t RepOrDiv -> m (Dynamic t (GeneralPattern a, Event t (EditSignal a)))
clickListWidget cycleMap (Atom iVal iReps) updatedReps = mdo
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVal) $ elems cycleMap
  sampleButton <- tdButtonAttrs' showVal (iVal) $ "class"=:"clickListtd"
  num <- count sampleButton >>= mapDyn (\x-> (x+initialNum) `mod` length cycleMap)
  str'' <- mapDyn (\x-> maybe iVal id $ Data.Map.lookup x cycleMap) num
  let str' = updated str''
  str <- holdDyn (iVal) str'
  reps <- holdDyn (iReps) updatedReps
  returnSample <- combineDyn (\x r -> Atom x r) str reps
  showVal <- mapDyn show returnSample
  mapDyn (\x->(x,never)) returnSample

repDivWidget::MonadWidget t m => RepOrDiv -> m (Dynamic t RepOrDiv)
repDivWidget (Rep iVal) = elAttr "table" ("class"=:"repDivTable")$ mdo
  val <- el "tr" $ do
    repOrDiv <- el "td" $ mdo
      text "*"
      rep <- checkbox True $ def & checkboxConfig_setValue .~ (fmap not divideEv)
      let repEv = _checkbox_change rep
      el "br" blank
      text "/"
      divide <- checkbox False $ def & checkboxConfig_setValue .~ (fmap not repEv)
      let divideEv = _checkbox_change divide
      let val = _checkbox_value rep
      return val
    num <- el "td" $ do
      let attrs = fromList $ zip ["style","step","min"] ["width:35px;","1", "0"]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      forDyn inVal (\x-> maybe 1 id $ ((readMaybe x)::Maybe Int))
    combineDyn (\rd val -> if rd then Rep val else Div val) repOrDiv num
  return val
repDivWidget (Div iVal) = elAttr "table" ("class"=:"repDivTable")$ mdo
  val <- el "tr" $ do
    repOrDiv <- el "td" $ mdo
      text "*"
      rep <- checkbox False $ def & checkboxConfig_setValue .~ (fmap not divideEv)
      let repEv = _checkbox_change rep
      el "br" blank
      text "/"
      divide <- checkbox True $ def & checkboxConfig_setValue .~ (fmap not repEv)
      let divideEv = _checkbox_change divide
      let val = _checkbox_value rep
      return val
    num <- el "td" $ do
      let attrs = fromList $ zip ["style","step","min"] ["width:35px;","1", "0"]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      forDyn inVal (\x-> maybe 1 id $ ((readMaybe x)::Maybe Int))
    combineDyn (\rd val -> if rd then Rep val else Div val) repOrDiv num
  return val
repDivWidget _ = repDivWidget (Rep 1)


-- countStepWidget (see ICOAH 'up' Widget for example)
-- step is the amount each click of the up/down arrows modify the value by
--   ---------------
--   --  0.0   ▲  --
--   --   -    ▼  --
--   ---------------
countStepWidget::MonadWidget t m => Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
--countStepWidget step (Atom iUpVal _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
countStepWidget step (Atom iUpVal _) _ = elAttr "table" ("class"=:"countWidgetTable") $ mdo
  upCount <- el "tr" $ do
    elAttr "td" ("class"=:"countWidgetTable") $ dynText upValShow
    upButton <- tdButtonAttrs "▲" () ("class"=:"countWidgetTable-upArrowtd") >>= count
    return upButton
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solidblack"
    downButton <- tdButtonAttrs "▼" () ("class"=:"countWidgetTable-downArrowtd") >>= count
    return $ (deleteButton, downButton)
  upVal <- combineDyn (\a b ->  (a*step)-(b*step)+(iUpVal)) upCount downCount
  upValShow <- forDyn upVal show
  --repsHold <- holdDyn iUpVal $ updated repeats
  mapDyn (\x->(Atom x Once,deleteEvent)) upVal
countStepWidget step _ e = countStepWidget step (Atom 0 Once) e

-- widget with a slider returning a single Atom with range [minVal,maxVal] and stepsize specified
doubleSliderWidget::MonadWidget t m => (Double,Double) -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
doubleSliderWidget (minVal,maxVal) stepsize (Atom iEnd Once) _ = elAttr "table" ("class"=:"doubleSliderWidget") $ mdo
  slider <- el "tr" $ elAttr "td" (Data.Map.union ("colspan"=:"3") ("style"=:"text-align:left")) $ do
      let attrs = constDyn $ fromList $ zip ["min","max","step","style"] [show minVal,show maxVal, show stepsize,"width:100px"]
      rangeInput <- textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs & textInputConfig_setValue .~ sliderUpdateVal & textInputConfig_initialValue .~ (show iEnd)
      let rangeVal = _textInput_value rangeInput
      mapDyn (\x-> maybe 0.5 id (readMaybe x::Maybe Double)) rangeVal
  (begEv,endEv,delEv) <- el "tr" $ do
    begPlus <- tdButtonAttrs "<" (-0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
    endPlus <- tdButtonAttrs ">" (0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border: 1pt solid black"
    return (begPlus,endPlus,deleteButton)
  let buttons = leftmost [endEv,begEv]
  let sliderValBeh = current slider
  let sliderAndButtonVal = attachWith (\a b -> max 0 $ min 1 $ a+b) sliderValBeh buttons
  let sliderUpdateVal = fmap show sliderAndButtonVal
  mapDyn (\x-> (Atom x Once,delEv)) slider


-- < and > buttons, background color fill illustrateds value
-- see iclc fixed, end widget
--  -----------------
--  --  <   >   -  --
--  -----------------
faderButtonWidget::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
faderButtonWidget (Atom iEnd Once) _ = elAttr "td" ("style"=:"text-align:center;margin:10px") $ mdo
  (returnVal,attrs) <- elDynAttr "td" attrs $ do
    (begEv,endEv,delEv) <- el "tr" $ do
      begPlus <- tdButtonAttrs "<" (-0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
      endPlus <- tdButtonAttrs ">" (0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
      deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border: 1pt solid black"
      return (begPlus,endPlus,deleteButton)
    let buttons = leftmost [endEv,begEv]
    endVal' <- foldDyn (\a b-> min 1 $ max 0 $ a+b) iEnd buttons
    endVal <- mapDyn (\x-> (fromInteger $ round $ x*100)/100) endVal'
    endGradient <- forDyn endVal makeStyleString
    tableAttrs <- forDyn endGradient (\x->"style"=:("text-align:center;display:inline-table;width:100pt;border-spacing:5px;border:2pt solid black;"++x))
    val <- mapDyn (\x-> (Atom x Once,delEv)) endVal
    return $ (val, tableAttrs)
  return returnVal



charWidget'::MonadWidget t m => GeneralPattern Char-> Event t () -> m (Dynamic t (GeneralPattern Char,Event t (EditSignal a)))
charWidget' (Atom iVal reps) _ = do
  textField <-textInput $ def & textInputConfig_attributes .~ (constDyn $ fromList $ zip ["style", " maxlength"] ["width:40px", "1"]) & textInputConfig_initialValue .~ [iVal]
  let inputVal = _textInput_value textField
  inputChar <- mapDyn (\c-> if length c ==1 then c!!0 else  '~') inputVal
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\char rep-> if char=='~' then Blank else Atom char rep) inputChar repeats'
  forDyn genPat (\k-> (k,deleteButton))

-- used in charContainer, example in Vowel in ICLCStacked widget
charWidget::(MonadWidget t m) => GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t (EditSignal a)))
--charWidget (Atom iVal iReps) _ = elAttr "div" ("style"=:"display:inline-block;") $ elAttr "table" tableAttrs $ mdo
charWidget (Atom iVal iReps) _ = elAttr "table" ("class"=:"aGLStringWidgetTable") $ mdo
  genPat <- el "tr" $ do
    val <- el "td" $ do
      inputField <- textInput $ def & textInputConfig_attributes .~ constDyn ("class"=:"aGLNumberWidgetTable-textFieldtd") & textInputConfig_initialValue .~ ([iVal])
      let val = _textInput_value inputField
      forDyn val (\x-> maybe (' ') id $ listToMaybe x)
    reps <- repDivWidget iReps
    combineDyn (\x y -> Atom x y) val reps
  (layerEvent, groupEvent, deleteEvent) <- el "tr" $ elAttr "td" ("colspan"=:"3") $ el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLStringWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLStringWidgetTable-layertd")
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ ("class"=:"aGLStringWidgetTable-deletetd")
    return $ (layerButton, groupButton, deleteButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x-> (x,leftmost [rebuildEvent, deleteEvent])) genPat''
charWidget _ e = charWidget (Atom ' ' Once) e

intWidget::MonadWidget t m => GeneralPattern Int-> Event t () -> m (Dynamic t (GeneralPattern Int,Event t (EditSignal a)))
intWidget iVal _ = do
  let attrs = def & textInputConfig_attributes .~ constDyn ("style"=:"width:20px;") & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
  textField <-textInput attrs
  let inputVal = _textInput_value textField
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\str rep-> if isJust (readMaybe str::Maybe Int) then Atom (read str::Int) rep else Atom 0 rep ) inputVal repeats'
  forDyn genPat (\k-> (k,deleteButton))


-----------------------------------------------
--       MORE CONTEXT-SPECIFIC WIDGETS...    -- (ie. inteded to be used for 'crush' vs. generally applicable to 'int')
-----------------------------------------------

crushWidget::MonadWidget t m => GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t (EditSignal a)))
crushWidget iVal _ = do
  text "0"
  let attrs = constDyn $ fromList $ zip ["type","min","max","step","style"] ["range","0","16","1","width:75px"]
  slider<-textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs
  text "16"
  let panVal = _textInput_value slider
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Int) then Atom (read x::Int) Once else Atom 16 Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton))


---- uses clickListWidget as a base widget, intersperses with + buttons
sampleNameWidget::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t (EditSignal a)))
sampleNameWidget (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty)$ do
    (samp,_) <- clickListWidget (fromList $ zip [(1::Int)..] ["~","bd","cp","bassfoo","moog", "arpy"]) (Atom iSamp iReps) repeatsEv >>= splitDyn
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border 1pt solid black") >>= count
    return $ (samp,upButton)
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solidblack"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repInitial <- holdDyn iReps $ updated repeats'
  let repeatsEv = updated repInitial
  mapDyn (\x->(x,deleteEvent)) sample
  where tableAttrs=("style"=:"display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border: 3pt solid black")
sampleNameWidget _ e = sampleNameWidget (Atom "~" Once) e




---- Eldad's Widgets:
sButtonContainer::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t (EditSignal a)))
sButtonContainer (Atom iSamp iReps) _ = elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty)$ do
    (samp,_) <- sButtonWidget (Atom iSamp iReps) repeatsEv >>= splitDyn
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (samp,upButton)
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repInitial <- holdDyn iReps $ updated repeats'
  let repeatsEv = updated repInitial
  mapDyn (\x->(x,deleteEvent)) sample
  where tableAttrs=("style"=:"margin:5px;display:inline-table;background-color:lightgreen;width:10%;padding:6px;border-spacing:5px;border: 3pt solid black")
sButtonContainer _ e = sButtonContainer (Atom "~" Once) e

sButtonWidget::MonadWidget t m =>  GeneralPattern SampleName -> Event t RepOrDiv -> m (Dynamic t (GeneralPattern SampleName, Event t (EditSignal a)))
sButtonWidget (Atom iSamp iReps) updatedReps = mdo
  let sampleMap = fromList $ zip [(0::Int)..] ["~","bd","sn","cp","hh"]  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iSamp) $ elems sampleMap
  sampleButton <- tdButtonAttrs' (showSample) (iSamp) $ "style"=:"width:60%;text-align:center;background-color:lightblue"
  num <- count sampleButton >>= mapDyn (\x-> (x+initialNum) `mod` length sampleMap)
  str'' <- mapDyn (\x-> maybe ("~") id $ Data.Map.lookup x sampleMap) num
  let str' = updated str''
  str <- holdDyn (iSamp) str'
  reps <- holdDyn (iReps) updatedReps
  returnSample <- combineDyn (\x r -> Atom x r) str reps
  showSample <- mapDyn show returnSample
  mapDyn (\x->(x,never)) returnSample

-- returns atom of a character
vowelButtonWidget::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t (EditSignal a)))
vowelButtonWidget (Atom iVowel _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" ("style"=:("width:100px;border-spacing:5px;display:inline-table;background-color:lightgreen;border:1pt solid black")) $ mdo
  let vowMap = fromList $ zip [0::Int,1..] ['X','a','e','i','o','u']  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVowel) $ elems vowMap
  vowelButton <- tdButtonAttrs' showVowel iVowel $ "style"=:"width:50px;text-align:center;background-color:lightblue;border:1pt solid black"
  deleteButton <- tdButtonAttrs (" - ") (DeleteMe) $ "style"=:"width: 50px; text-align:center;background-color:lightblue;border:1pt solid black"
  num <- count vowelButton >>= mapDyn (\x-> (x+initialNum) `mod` length vowMap)
  char'' <- mapDyn (\x-> maybe ('X') id $ Data.Map.lookup x vowMap) num
  let char' = updated char''
  char <- holdDyn (iVowel) char'
  vowel <- mapDyn (\x -> Atom x Once) char
  showVowel <- mapDyn show vowel
  mapDyn (\x->(x,deleteButton)) vowel



-- For faderButtonWidget - gradient string used to show the
makeStyleString gradient =
  "background: -webkit-linear-gradient(90deg,lightgreen "++ (show $ x+1) ++ "%, white "++(show $ x) ++ "%);"++
    "background: -o-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x)++ "%);" ++
      "background: -moz-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x) ++ "%);" ++
        "background: linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white " ++ (show x) ++ "%);"
  where x = gradient*100
