{-# LANGUAGE RecursiveDo, OverloadedStrings, TypeFamilies #-}

module Estuary.Widgets.GeneralPattern where

import Reflex
import Reflex.Dom hiding (Insert,Delete)
import Data.Bool (bool)
import Data.Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Data.List(intersperse, findIndex, elem, elemIndex)
import Data.Either(partitionEithers)
import GHCJS.DOM.EventM
import Data.Maybe(isJust,listToMaybe,fromMaybe,fromJust)
import Text.Read(readMaybe)
import Control.Applicative (liftA2)

import Estuary.Types.Hint
import Estuary.Types.Live
import Estuary.Widgets.Reflex
import Estuary.Tidal.Types



generalContainerLive' :: (MonadWidget t m, Eq a, Show a)
  => (Dynamic t Liveness -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint)))
  -> GeneralPattern a
  -> Event t (EditSignal (GeneralPattern a))
  -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
generalContainerLive' b i e= mdo
  gpat <- resettableWidget (generalContainerLive b) i e rebuildEvent'
  let ev = switchPromptlyDyn $ fmap (\(_,x,_)->x) gpat
  let rebuildEvent = ffilter (\x->case x of RebuildMe -> True; otherwise->False) ev
  let rebuildEvent' = attachPromptlyDynWith (\(v,_,_) ev->v) gpat rebuildEvent
  return gpat



generalContainerLive :: (MonadWidget t m, Eq a, Show a)
  => (Dynamic t Liveness -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint)))
  -> GeneralPattern a
  -> Event t (EditSignal (GeneralPattern a))
  -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
generalContainerLive b i _ = elClass "div" (getClass i) $ mdo
  text "["
  let cEvents = mergeWith (union) [insertMap,deleteMap]
  (allValues,events,hints) <- eitherContainer4 (initialMap i) cEvents livenessEvMap livenessEvMap (leftBuilder liveness) (rightBuilder liveness)
  let deleteMap = fmap (fromList . concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)]) . keys . Data.Map.filter (==DeleteMe)) events
  let insertMap = fmap (fromList . concat . (insertList (Blank Inert)) . keys . Data.Map.filter isChangeValue )  events

  let values = fmap (fst . Data.Either.partitionEithers . elems) allValues
  let childKeys = fmap keys allValues
  let events' = fmap (Data.Map.elems) events -- Event [l]

  let potential = constDyn Inert

  let livenessEv = fmap (\x-> if Data.List.elem MakeL3 x then L3 else L4) $ ffilter (\x-> Data.List.elem MakeL3 x || Data.List.elem MakeL4 x) events' -- If any child reports a liveness change
  let livenessEvMap = attachPromptlyDynWith (\k v -> fromList $ zip k $ repeat (case v of L3 -> MakeL3; otherwise -> MakeL4)) childKeys livenessEv -- Ev (Map Int MakeL3) -- cycled back to the container to change all children to appropriate liveness
  liveness <- holdDyn L4 livenessEv
  let evalEv = (Eval <$) $ ffilter (\x-> Data.List.elem Eval x) events' -- If any child reports a change
  let groupLayerEv = (RebuildMe <$) $ ffilter (\x-> Data.List.elem MakeGroup x || Data.List.elem MakeLayer x) events'
  let deleteContainerEv = (DeleteMe <$) $ ffilter (\x -> Data.List.elem DeleteContainer x) events'

  unchangedVal <- holdDyn (fst . Data.Either.partitionEithers . elems $ initialMap i) $ tagPromptlyDyn values $ leftmost [evalEv, fmap (\x-> if x==L3 then MakeL3 else MakeL4) livenessEv]

  let oldAndNew = (,) <$> unchangedVal <*> values
  let live = (\l (oldVal, newVal) ->if l==L4 then Live (newVal,Once) L4 else Edited (oldVal,Once) (newVal,Once)) <$> liveness <*> oldAndNew

  let splitIndex = coincidence $ fmap (const $ fmap (head . keys . Data.Map.filter (==LayerSplit)) events) $ ffilter (Data.List.elem LayerSplit) events'

  let splitVal = attachPromptlyDynWith (\l index-> let (vals,rep) = case l of (Edited (old,reps) new) -> (old,reps); (Live (v,reps) _) -> (v,reps); in Layers (Live ([Group (Live (Prelude.take (div index 2) vals,Once) L4) Inert, Group (Live (reverse $ Prelude.take ((length vals) - (div index 2)) (reverse vals), Once) L4) Inert],rep) L4) Inert) live splitIndex
  let returnEvents = (leftmost [fmap (\x-> if x==L3 then MakeL3 else MakeL4) livenessEv, deleteContainerEv, (RebuildMe <$) $ ffilter (Data.List.elem LayerSplit) events',groupLayerEv])
  let retVal = (\l p-> returnF i l p returnEvents hints) <$> live <*> potential
  let regVal = fmap (\(x,_,_)->x) retVal

  updatedRegVal <- liftM join $ holdDyn regVal $ fmap constDyn splitVal
  text "]"
  return $ (\reg (_,e,h)->(reg,e,h)) <$> updatedRegVal <*> retVal
  where
    initialVal (Atom iV _ _) = iV
    initialVal (Group (Live (iV,_) _) _) = initialVal $ iV!!0
    initialVal (Group (Edited (iV,_) _) _) = initialVal $ iV!!0
    initialVal (Layers (Live (iV,_) _) _) = initialVal $ iV!!0
    initialVal (Layers (Edited (iV,_) _) _) = initialVal $ iV!!0
    getClass (Layers _ _) = "generalPattern-group-or-layer-or-atom code-font background primary-color"
    getClass (Group _ _) = "generalPattern-group-or-layer-or-atom code-font background primary-color"
    getClass (Atom _ _ _) = "generalPattern-group-or-layer-or-atom code-font background primary-color"
    initialMap (Layers (Live (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Layers (Edited (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Group (Live (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Group (Edited (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Atom iVal p iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal p iReps, Right ()]
    leftBuilder live = aGLWidgetLive live b
    rightBuilder live = whitespace live (i) "whiteSpaceAdd" [ChangeValue (Blank Inert),DeleteContainer]
    insertList (Atom iVal p r) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (iVal) p r))])
    insertList (Layers (Edited (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Layers (Live (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Group (Edited (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Group (Live (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Blank p) = Prelude.map (\k -> [(k,Insert (Right ())),  (k+1, Insert (Left $ Blank p))])
    returnF (Layers _ _) x p e h= (Layers x p, e,h)
    returnF (Group _ _) x p e h= (Group x p,e,h)
    returnF (Atom _ _ _) x p e h= (Group x p,e,h)

aGLWidgetLive::(MonadWidget t m, Eq a, Show a) => Dynamic t Liveness -> (Dynamic t Liveness -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))) -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
aGLWidgetLive liveness builder iVal ev = mdo
  val <- resettableWidget (function) iVal ev rebuildEvent'
  let widgetEvents = switchPromptlyDyn $ fmap (\(_,y,_)->y) val
  let rebuildEvent = ffilter (\x->case x of RebuildMe -> True; otherwise->False) widgetEvents
  --holdDyn "no rebuild yet" (("event" <$) rebuildEvent) >>= dynText
  let rebuildEvent' = attachPromptlyDynWith (\(value,_,_) _ ->value) val rebuildEvent
  return val
  where
    function (Atom x p r) e = builder liveness (Atom x p r) e
    function (Blank p) e = builder liveness (Blank p) e
    function (Group l p) e = generalContainerLive (builder) (Group l p) e
    function (Layers l p) e = generalContainerLive  (builder) (Layers l p) e





typedAtomWidget:: (MonadWidget t m, Show a, Eq a, Read a) => a ->  Dynamic t Liveness -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
typedAtomWidget defaultVal  liveness iGenPat editEv = elClass "div" "atomPopup" $ mdo
  let (iVal,iRepDiv,iPotential) = getIVal iGenPat

  inputMouseOver <- liftM (True <$) $ wrapDomEvent (_textInput_element textField) (onEventName Mouseover) mouseXY
  inputMouseOut <- liftM (False <$) $ wrapDomEvent (_textInput_element textField) (onEventName Mouseout) mouseXY
  isMouseOverInput <- holdDyn False $ leftmost [inputMouseOut, inputMouseOver]
  let inputAttrs = fmap (fromList . (\x-> [x]) . ((,) "class") . bool "atomPopupInput primary-color background code-font other-borders" "atomPopupInputMouseOver primary-color background code-font other-borders") isMouseOverInput
  textField <- growingTextInput $ def & textInputConfig_attributes .~ inputAttrs & textInputConfig_initialValue .~ (T.pack $ showNoQuotes iVal)

  let textInputChange = updated $ _textInput_value textField
  onClickEv <- wrapDomEvent (_textInput_element textField) (onEventName Click) (mouseXY)
  let iRepDivViewable = (iRepDiv/=Once)
  repOrDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (basicPopup liveness popupActions) (case iPotential of Inert->False; otherwise->True) $ updated popupToggle
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
  repOrDiv <- holdDyn iRepDiv repOrDivEv >>= (\x -> return $ (\tog val-> if tog then val else Once) <$> repDivToggle <*> x)
  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) popupMenu
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4|| x==Just Eval) popupMenu
  let groupEv = fmap (\x->case x of (Just MakeGroup)->MakeGroup;otherwise->Close) $ ffilter (\x->case x of (Just MakeGroup)->True;otherwise->False) popupMenu
  let layerEv = fmap (\x->case x of (Just MakeLayer)->MakeLayer;otherwise->Close) $ ffilter (\x->case x of (Just MakeLayer)->True;otherwise->False) popupMenu
  popupToggle <- holdDyn (case iPotential of Inert->False;otherwise->True) $ leftmost [False <$ textInputChange, ffilter id $ updated $ _textInput_hasFocus textField, False <$ popupMenu, True <$ onClickEv]
  -- This is kind of an ugly hack to deal with parsing a string in a string, but allows for the general 'a' in the type signature here
  let inVal = fmap (\x-> if ((maybe defaultVal id $ readMaybe (T.unpack x)) == defaultVal) then maybe defaultVal id (readMaybe $ show (T.unpack x)) else maybe defaultVal id (readMaybe (T.unpack x))) $ _textInput_value textField
  potential <- do
    let x = fmap (\x-> if x then Potentials (fmap toPotential popupActions) else Inert) popupToggle
    holdDyn iPotential $ updated x
  atomVal <- (return (Atom <$> inVal <*> potential)) >>= (\x -> return $ (\r con ->con r) <$> repOrDiv <*> x)
  let groupVal = (\v l -> if l==L4 then Group  (Live ([v],Once) L4) Inert  else Group (Edited ([v],Once) ([v],Once)) Inert) <$> atomVal <*> liveness
  let layerVal = (\v l -> if l==L4 then Layers (Live ([v],Once) L4) Inert  else Layers (Edited ([v],Once) ([v],Once)) Inert) <$> atomVal <*> liveness
  let groupEvVal = tag (current groupVal) groupEv
  let layerEvVal = tag (current layerVal) layerEv
  genPat <- liftM join $ holdDyn atomVal $ leftmost $ fmap (fmap constDyn) [groupEvVal,layerEvVal]
  let upSig = fmap toEditSigGenPat $ leftmost [deleteEv, livenessEv,(RebuildMe <$) groupEv, (RebuildMe <$) layerEv]
  return $ fmap (\x-> (x,upSig, never)) genPat
  where
    popupActions = [MakeRepOrDiv, MakeGroup, MakeLayer, DeleteMe]
    getIVal gP = case gP of
      (Group (Live (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Group (Edited (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Atom v p r) -> (v,r,p)
      otherwise -> (defaultVal , Once,Inert)


popupIntWidget :: MonadWidget t m => Int -> Int -> Int -> Int -> Dynamic t Liveness -> GeneralPattern Int -> Event t (EditSignal (GeneralPattern Int)) -> m (Dynamic t (GeneralPattern Int, Event t (EditSignal (GeneralPattern Int)), Event t Hint))
popupIntWidget defaultVal minVal maxVal step liveness iGenPat editEv = elClass "div" "atomPopup" $ mdo
  let (iVal,iRepDiv,iPotential) = getIVal iGenPat
  textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (T.pack $ show iVal) & textInputConfig_inputType .~"number" & textInputConfig_attributes .~ (constDyn ("style"=:"width:30px; border: none"))
  let iRepDivViewable = (iRepDiv/=Once)
  repOrDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (popup) (case iPotential of Inert->False; otherwise->True) $ updated popupToggle
  let popupEv = leftmost $ [ffilter id $ updated $ _textInput_hasFocus textField, closeEvent]
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
  repOrDiv <- holdDyn iRepDiv repOrDivEv >>= (\x -> return $ (\tog val-> if tog then val else Once) <$> repDivToggle <*> x)
  let closeEvent = (False <$)  $ ffilter (isNothing)  popupMenu
  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) popupMenu
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4|| x==Just Eval) popupMenu
  let groupEv = fmap (\x->case x of (Just MakeGroup)->MakeGroup;otherwise->Close) $ ffilter (\x->case x of (Just MakeGroup)->True;otherwise->False) popupMenu
  let layerEv = fmap (\x->case x of (Just MakeLayer)->MakeLayer;otherwise->Close) $ ffilter (\x->case x of (Just MakeLayer)->True;otherwise->False) popupMenu
  popupToggle <- toggle (case iPotential of Inert->False; otherwise->True) $ leftmost [(()<$) popupMenu,(()<$) livenessEv , (()<$) $ ffilter id $ updated $ _textInput_hasFocus textField,(() <$)closeEvent,(() <$) groupEv, (() <$) layerEv]
  let inVal = fmap (maybe iVal id . (readMaybe::String -> Maybe Int) . T.unpack) $ _textInput_value textField
  potential <- liftM updated (return $ fmap (\x-> if x then Potentials (fmap toPotential popupActions) else Inert) popupToggle) >>= holdDyn iPotential
  atomVal <- do
    let x = Atom <$> inVal <*> potential
    return $ (\r con ->con r) <$> repOrDiv <*> x
  let groupVal = (\v l -> if l==L4 then Group  (Live ([v],Once) L4) Inert  else Group (Edited ([v],Once) ([v],Once)) Inert) <$> atomVal <*> liveness
  let layerVal = (\v l -> if l==L4 then Layers (Live ([v],Once) L4) Inert  else Layers (Edited ([v],Once) ([v],Once)) Inert) <$> atomVal <*> liveness
  let groupEvVal = tag (current groupVal) groupEv
  let layerEvVal = tag (current layerVal) layerEv
  genPat <- liftM join $ holdDyn atomVal $ leftmost $ fmap (fmap constDyn) [groupEvVal,layerEvVal]
  let upSig = fmap toEditSigGenPat $ leftmost [deleteEv, livenessEv,(RebuildMe <$) groupEv, (RebuildMe <$) layerEv]
  return $ fmap (\x-> (x,upSig, never)) genPat
  where
    attrs = fromList $ zip ["class","step","min","max"] ["atomPopupInput primary-color background",T.pack $ show step, T.pack $ show minVal, T.pack $ show maxVal]
    popupActions = [MakeRepOrDiv::EditSignal Int, MakeGroup, MakeLayer, DeleteMe]
    popup = basicPopup liveness popupActions
    getIVal gP = case gP of
      (Group (Live (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Group (Edited (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Atom v p r) -> (v,r,p)
      otherwise -> (defaultVal, Once,Inert)


charWidget::(MonadWidget t m) => Dynamic t Liveness -> GeneralPattern Char -> Event t (EditSignal (GeneralPattern Char)) -> m (Dynamic t (GeneralPattern Char, Event t (EditSignal (GeneralPattern Char)),Event t Hint))
charWidget _ iGenPat _ = elAttr "table" ("class"=:"aGLStringWidgetTable") $ mdo
  let (iVal,iReps) = getIVal iGenPat
  genPat <- el "tr" $ do
    val <- el "td" $ do
      inputField <- textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:45px;") & textInputConfig_initialValue .~ (T.singleton iVal)
      let val = _textInput_value inputField
      return $ ffor val (\x-> maybe (' ') id $ listToMaybe (T.unpack x))
    reps <- repDivWidget iReps
    return $ (\x y -> Atom x Inert y) <$> val <*> reps
  (layerEvent, groupEvent, deleteEvent) <- el "tr" $ elAttr "td" ("colspan"=:"3") $ el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("style"=:"text-align:center;")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("style"=:"text-align:center;")
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ ("style"=:"width:45px; text-align:center;")
    return $ (layerButton, groupButton, deleteButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- return $ (\u tog-> if tog then Group (Live ([u],Once) L4) Inert else u) <$> genPat <*> groupToggle
  genPat''<- return $ (\u tog-> case u of (Atom a Inert x) -> if tog then Layers (Live ([u],Once) L4) Inert else u; otherwise-> u) <$> genPat' <*> layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  return $ fmap (\x-> (x,leftmost [rebuildEvent, deleteEvent],never)) genPat''
  where
  getIVal i = case i of
    (Atom a _ r) -> (a,r)
    (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)

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
      let attrs = fromList $ zip ["class", "style","step","min"] ["primary-color background code-font other-borders", "width:35px;","1", "0"]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (T.pack $ show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      return $ ffor inVal (\x-> maybe 1 id $ ((readMaybe (T.unpack x))::Maybe Int))
    return $ (\rd val -> if rd then Rep val else Div val) <$> repOrDiv <*> num
  return val
repDivWidget (Div iVal) = elAttr "table" ("class"=:"repDivTable primary-color background code-font other-borders")$ mdo
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
      let attrs = fromList $ zip ["class", "style","step","min"] ["primary-color background code-font other-borders", "width:35px;","1", "0"]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (T.pack $ show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      return $ ffor inVal (\x-> maybe 1 id $ ((readMaybe (T.unpack x))::Maybe Int))
    return $ (\rd val -> if rd then Rep val else Div val) <$> repOrDiv <*> num
  return val
repDivWidget _ = repDivWidget (Rep 1)

popupDoubleWidget :: MonadWidget t m => Double -> Double -> Double -> Double -> Dynamic t Liveness -> GeneralPattern Double -> Event t (EditSignal (GeneralPattern Double)) -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal (GeneralPattern Double)), Event t Hint))
popupDoubleWidget defaultVal minVal maxVal step liveness iGenPat editEv = elClass "div" "atomPopup primary-color background code-font" $ mdo
  let (iVal,iRepDiv,iPotential) = getIVal iGenPat
  textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (T.pack $ show iVal) & textInputConfig_inputType .~"number" & textInputConfig_attributes .~ (constDyn ("style"=:"width:30px; border: none;"))
  let iRepDivViewable = (iRepDiv/=Once)
  repOrDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (popup) (case iPotential of Inert->False; otherwise->True) $ updated popupToggle
  let popupEv = leftmost $ [ffilter id $ updated $ _textInput_hasFocus textField, closeEvent]
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
  repOrDiv <- holdDyn iRepDiv repOrDivEv >>= (\x -> return $ (\tog val-> if tog then val else Once) <$> repDivToggle <*> x)
  let closeEvent = (False <$)  $ ffilter (isNothing)  popupMenu
  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) popupMenu
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4|| x==Just Eval) popupMenu
  let groupEv = fmap (\x->case x of (Just MakeGroup)->MakeGroup;otherwise->Close) $ ffilter (\x->case x of (Just MakeGroup)->True;otherwise->False) popupMenu
  let layerEv = fmap (\x->case x of (Just MakeLayer)->MakeLayer;otherwise->Close) $ ffilter (\x->case x of (Just MakeLayer)->True;otherwise->False) popupMenu
  popupToggle <- toggle (case iPotential of Inert->False; otherwise->True) $ leftmost [(()<$) popupMenu,(()<$) livenessEv , (()<$) $ ffilter id $ updated $ _textInput_hasFocus textField,(() <$)closeEvent,(() <$) groupEv, (() <$) layerEv]
  let inVal = fmap (maybe iVal id . (readMaybe::String -> Maybe Double) . T.unpack) $ _textInput_value textField
  potential <- liftM updated (return $ fmap (\x-> if x then Potentials (fmap toPotential popupActions) else Inert) popupToggle) >>= holdDyn iPotential
  atomVal <- (return $ (\val pot -> Atom val pot) <$> inVal <*> potential) >>= (\x -> return $ (\r con ->con r) <$> repOrDiv <*> x)
  let groupVal = (\v l -> if l==L4 then Group  (Live ([v],Once) L4) Inert  else Group (Edited ([v],Once) ([v],Once)) Inert) <$> atomVal <*> liveness
  let layerVal = (\v l -> if l==L4 then Layers (Live ([v],Once) L4) Inert  else Layers (Edited ([v],Once) ([v],Once)) Inert) <$> atomVal <*> liveness
  let groupEvVal = tag (current groupVal) groupEv
  let layerEvVal = tag (current layerVal) layerEv
  genPat <- liftM join $ holdDyn atomVal $ leftmost $ fmap (fmap constDyn) [groupEvVal,layerEvVal]
  let upSig = fmap toEditSigGenPat $ leftmost [deleteEv, livenessEv,(RebuildMe <$) groupEv, (RebuildMe <$) layerEv]
  return $ fmap (\x-> (x,upSig, never)) genPat
  where
    attrs = fromList $ zip ["class","step","min","max"] ["atomPopupInput primary-color background",T.pack $ show step, T.pack $ show minVal, T.pack $ show maxVal]
    popupActions = [MakeRepOrDiv::EditSignal Double, MakeGroup, MakeLayer, DeleteMe]
    popup = basicPopup liveness popupActions
    getIVal gP = case gP of
      (Group (Live (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Group (Edited (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Atom v p r) -> (v,r,p)
      otherwise -> (defaultVal, Once,Inert)
