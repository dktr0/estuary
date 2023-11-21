{-# LANGUAGE RecursiveDo, OverloadedStrings, FlexibleContexts #-}

module Estuary.Widgets.Reflex where

-- This module is for definitions that extend the affordances of reflex and reflex-dom in
-- ways that are specific to the Estuary client but generically useful across multiple
-- widgets/settings/modules within the client.

import Reflex
import Reflex.Dom hiding (Delete,Insert)
import Data.Map
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM.EventM
import qualified GHCJS.DOM.Types as G
import qualified GHCJS.DOM.GlobalEventHandlers as G
import qualified GHCJS.DOM.DocumentAndElementEventHandlers as G
import Data.Map
import Data.Maybe
import Data.List
import Data.Bool (bool)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix (MonadFix)
import GHCJS.DOM.HTMLSelectElement as Select
import Safe -- for readMay
import GHCJS.DOM.GlobalEventHandlers (change)
import Data.List (nub, elemIndex)
import Text.Read (readMaybe)
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal.Pure as P
import Data.Functor.Misc -- For Const2

import Estuary.Types.Term
import Estuary.Types.Language
import Estuary.Types.TranslatableText
import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Types.Hint


-- the former 'dynEditor'...
dyn' :: (Monad m, MonadSample t m, Reflex t, Adjustable t m, MonadHold t m) => Dynamic t (m a) -> m (Dynamic t a)
dyn' x = do
  initialWidget <- sample $ current x
  widgetHold initialWidget $ updated x -- m (Dynamic t a)

-- for dynamic attributes
dynAttr :: Reflex t => Text -> Dynamic t Text -> Dynamic t (Map Text Text)
dynAttr k = fmap (Data.Map.singleton k)

-- a temporary button with class for the reference files
buttonWithClass' :: (Monad m, Reflex t, DomBuilder t m) => Text -> m (Event t ())
buttonWithClass' s = do
  (e, _) <- elAttr' "button" (fromList [("type", "button"), ("class", "ui-buttons code-font primary-color"), ("style", "background-color:transparent; border:none; cursor:help")]) $ text s
  return $ domEvent Click e

-- a button with class
buttonWithClass :: (Monad m, Reflex t, DomBuilder t m) => Text -> m (Event t ())
buttonWithClass s = do
  (e, _) <- elAttr' "button" (fromList [("type", "button"), ("class", "ui-buttons other-borders code-font")]) $ text s
  return $ domEvent Click e

buttonWithSettableClass :: (Monad m, Reflex t, DomBuilder t m) => Text -> Text -> m (Event t ())
buttonWithSettableClass c s = do
  (e, _) <- elAttr' "button" (fromList [("type", "button"), ("class", c)]) $ text s
  return $ domEvent Click e

-- used in the footer
invisibleButton :: (Monad m, Reflex t, DomBuilder t m) => m (Event t ())
invisibleButton = do
  (e, _) <- elAttr' "button" (fromList [("type", "button"), ("class", "invisible-button")]) $ text "none"
  return $ domEvent Click e

--button with dynamic label and settable class

dynButtonWSettableClass :: (Adjustable t m, DomBuilder t m, PostBuild t m, MonadHold t m) => Dynamic t Text -> Dynamic t Text -> m (Event t ())
dynButtonWSettableClass c s = dynE (buttonWithSettableClass <$> c <*> s)


-- dynButtonWSettableClass' :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> Dynamic t (m (Event t a))
-- dynButtonWSettableClass' c s = buttonWithSettableClass <$> c <*> s
--

--Button with dynamic label.
dynButton :: (Adjustable t m, DomBuilder t m, PostBuild t m, MonadHold t m) => Dynamic t Text -> m (Event t ())
dynButton = dynE . fmap buttonWithClass

dynButtonWithChild :: (Monad m, Reflex t, DomBuilder t m) => String -> m () -> m (Event t ())
dynButtonWithChild cls child = do
  (e, _) <- elAttr' "div" (fromList [("type", "button"), ("class", T.pack $ cls ++ " btn")]) child
  return $ domEvent Click e

-- | dynE is like dyn from Reflex, specialized for widgets that return
-- events. A dynamic argument updates the widget, and the return value is
-- already flattened to just being the events returned by the child widget.
dynE :: (Adjustable t m, Monad m, Reflex t, NotReady t m, PostBuild t m, MonadHold t m) => Dynamic t (m (Event t a)) -> m (Event t a)
dynE x = dyn x >>= switchHoldPromptly never

-- a button that, instead of producing Event t (), produces an event of
-- some constant value
button' :: (Monad m, DomBuilder t m, Reflex t) => Text -> a -> m (Event t a)
button' t r = do
  x <- button t
  return (r <$ x)

-- Button With Dynamic attributes
buttonDynAttrs :: (Monad m, DomBuilder t m, Reflex t, PostBuild t m) => Text -> a -> Dynamic t (Map Text Text)-> m (Event t a)
buttonDynAttrs s val attrs = do
  (e, _) <- elDynAttr' "button" attrs $ text s
  let event = domEvent Click e
  return (val <$ event)

-- Creates dropdown Menu with Subheaders
-- takes a Map of integers (the order everything should be displayed in)
-- to String tuples. The first String of the tuple indicates a subheader,
-- and the second indicates the selectable item under it. DropdownConfig options
-- expect the same as with a regular dropdown
-- dropdownOpts :: (Monad m, DomBuilder t m, Reflex t, Adjustable t m, PerformEvent t m, TriggerEvent t m, MonadIO m, MonadFix m, PostBuild t m, MonadHold t m) =>
dropdownOpts :: (Monad m, DomBuilder t m, Reflex t, Adjustable t m, PerformEvent t m, Reflex t, TriggerEvent t m, PostBuild t m, MonadFix m, MonadIO (Performable m), MonadIO m, MonadHold t m, G.IsElement (RawElement (DomBuilderSpace m)))
  => Int -> Map Int (Text,Text) ->  DropdownConfig t Int -> m (Dropdown t Int)
dropdownOpts k0 setUpMap (DropdownConfig setK attrs) = do
  let options = fromList $ Prelude.zip (keys setUpMap) $ fmap snd $ elems setUpMap
  let optGroups = fromList $ Prelude.zip (keys setUpMap) $ fmap fst $ elems setUpMap
  let optGroupPositions = fmap (\x-> maybe (0) id $ Data.List.elemIndex x (elems optGroups)) $ nub $ elems optGroups -- [Int]
  (eRaw, _) <- elDynAttr' "select" attrs $ do
    let optionsWithDefault = constDyn $ if Data.Map.lookup k0 options == Nothing then Data.Map.union (k0 =: "") options else options
    listWithKey optionsWithDefault $ \k v -> do
      if not (elem k optGroupPositions) then blank else do
        elAttr "optgroup" ("label"=:(maybe "" id $ Data.Map.lookup k optGroups)) $ blank
      elAttr "option" ("value" =: (T.pack . show) k <> if k == k0 then "selected" =: "selected" else mempty) $ dynText v
  let e = G.uncheckedCastTo HTMLSelectElement $ _element_raw eRaw
  performEvent_ $ fmap (Select.setValue e . show) setK
  eChange <- wrapDomEvent e (`on` change) $ do
--    kStr <- fromMaybe "" <$> Select.getValue e
    kStr <- Select.getValue e
    return $ readMay kStr
  let readKey mk = fromMaybe k0 $ do
        k <- mk
        guard $ Data.Map.member k options
        return k
  dValue <- (return . fmap readKey) =<< holdDyn (Just k0) (leftmost [eChange, fmap Just setK])
  return $ Dropdown dValue (fmap readKey eChange) -- @clean this.


-- below this line from the former Estuary.Widgets.Generic

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


isChangeValue :: EditSignal a -> Bool
isChangeValue (ChangeValue _) = True
isChangeValue _ = False
--data EditSignal = DeleteMe | MakeGroup |

justChangeValues :: EditSignal a -> Maybe a
justChangeValues (ChangeValue x) = Just x
justChangeValues _ = Nothing

--clickableDivDynAttrsWChild :: (Monad m, DomBuilder t m, TriggerEvent t m, MonadIO m, PostBuild t m)
clickableDivDynAttrsWChild :: (Monad m, DomBuilder t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, PostBuild t m, MonadIO m)
  => Dynamic t (Map Text Text) -> m a -> m (Event t ()) -- return (Event t (), a)
clickableDivDynAttrsWChild attrs child = do
  (element,_) <- elDynAttr' "div" attrs $ child
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (() <$) clickEv

--clickableDivNoClass :: (Monad m, TriggerEvent t m, Reflex t, DomBuilder t m, MonadIO m)
clickableDivNoClass :: (Monad m, DomBuilder t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, MonadIO m)
  => m a -> m (Event t a) -- return (Event t (), a)
clickableDivNoClass child = do
  (element, a) <- el' "div" $ child -- look elAttr' ::
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  let event = (a <$) clickEv
  return event

-- clickableDivWithLabel :: MonadWidget t m => Text -> a -> m (Event t a,  a)
-- clickableDivWithLabel label e = liftM (e <$) $ clickableDivNoClass $ text label

-- clickableA :: MonadWidget t m => Text -> m a -> m (Event t (), a)
-- clickableA label child = liftM (child <$) $ (clickableDivWithLabel $ text label)
   -- return (event, child)
  -- (element, a) <- el' "div" $ child
  -- clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  -- let event = (() <$) clickEv
  -- return $ a


-- clickableDivWithLabel :: MonadWidget t m => m a -> m (Event t ())
-- clickableDivWithLabel child = do
--   (element,_) <- el' "div" $ child
--   clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
--   return $ (() <$) clickEv

    
-- clickableDiv with class
clickableDiv :: (Monad m, DomBuilder t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, MonadIO m)
  => Text -> m a -> m (Event t ())
clickableDiv cssclass child = do
  (element,_) <- elAttr' "div" attr $ child
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (() <$) clickEv
  where
    attr = singleton "class" cssclass

clickableDivClass :: (Monad m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, DomBuilder t m, MonadIO m)
  => Text -> Text -> m (Event t ())
clickableDivClass label c = do
  (element,_) <- elAttr' "div" (singleton "class" c) $ text label
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (() <$) clickEv

clickableDivClass' :: (Monad m, Reflex t, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), DomBuilder t m, MonadIO m)
  => Text -> Text -> a -> m (Event t a)
clickableDivClass' label c e = liftM (e <$) $ clickableDivClass label c

-- with displayed text that can change
clickableDivClass'':: (MonadFix m, DomBuilder t m, PostBuild t m, Reflex t, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), MonadHold t m, MonadIO m)
  => Dynamic t Text -> Text -> a -> m (Event t a)
clickableDivClass'' label c e = do
  (element, _) <- elAttr' "div" ("class"=:c) $ dynText label
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (e <$) clickEv

mouseOverClickableDiv :: (MonadFix m, DomBuilder t m, Reflex t, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), PostBuild t m, MonadIO m, MonadHold t m)
  => Dynamic t Text -> Text -> a -> m(Event t a)
mouseOverClickableDiv label c e = mdo
  (element, _) <- elDynAttr' "div" attrs $ dynText label
  mouseOver <- liftM (True <$) $ wrapDomEvent (_element_raw element) (elementOnEventName Mouseover) mouseXY
  mouseOut <- liftM (False <$) $ wrapDomEvent (_element_raw element) (elementOnEventName Mouseout) mouseXY
  isMouseOver <- holdDyn False $ leftmost [mouseOut, mouseOver]
  let attrs  = fmap (fromList . (\x-> [("class",c),x]) . ((,) "style") . bool "" ";background-color:rgba(144,238,144,0.2);") isMouseOver
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (e <$) clickEv


clickableDivAttrs :: (Monad m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, DomBuilder t m, MonadIO m)
  => Text -> a -> Map Text Text -> m (Event t a)
clickableDivAttrs label val attrs= do
  (element,_) <- elAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (val <$) clickEv

clickableDivAttrs' :: (Monad m, TriggerEvent t m, Reflex t, G.IsElement (RawElement (DomBuilderSpace m)), DomBuilder t m, MonadIO m)
  => Text -> a -> Map Text Text -> x -> y -> m (Dynamic t ((),Event t a))
clickableDivAttrs' label val attrs _ _= do
  (element,_) <- elAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  let event = (val <$) clickEv
  return $ constDyn ((),event)

clickableDivDynAttrs :: (Monad m, DomBuilder t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, PostBuild t m, MonadIO m)
  => Text -> a -> Dynamic t (Map Text Text) -> m (Event t a)
clickableDivDynAttrs label val attrs = do
  (element,_) <- elDynAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (val <$) clickEv

-- with displayed text that can change
clickableSpanClass :: (Monad m, DomBuilder t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, PostBuild t m, MonadIO m)
  => Dynamic t Text -> Text -> a -> m (Event t a)
clickableSpanClass label c e = do
  (element, _) <- elAttr' "span" ("class"=:c) $ dynText label
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (e <$) clickEv

clickableTdClass :: (Monad m, DomBuilder t m, Reflex t, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), PostBuild t m, MonadIO m)
  => Dynamic t Text -> Dynamic t Text -> a -> m (Event t a)
clickableTdClass label c val = do
  let attrs = fmap (singleton "class") c
  (element, _) <- elDynAttr' "td" attrs $ dynText label
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ ((val) <$) clickEv


pingButton :: (Monad m, Reflex t, DomBuilder t m) => Text -> m (Event t ())
pingButton label = liftM (() <$) $ button label

pingButton' :: (Monad m, DomBuilder t m) => Text -> m (Dynamic t ((),Event t ()))
pingButton' label = do
  x <- pingButton label
  return $ constDyn ((),x)

pingButton'' :: (DomBuilder t m) => Text -> a -> b -> m (Dynamic t ((),Event t ()))
pingButton'' label _ _ = pingButton' label

pingButton''' :: (PostBuild t m, Monad m, DomBuilder t m) => Text -> Map Text Text -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())))
pingButton''' label attrs _ _ = do
  b <- buttonDynAttrs label (ChangeValue ()) $ constDyn attrs
  return $ constDyn ((), b)

makeNewButton :: (Monad m, DomBuilder t m, Reflex t) => Text -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())) )
makeNewButton label _ _ = do
  a <- button label
  return $ constDyn ((), ((MakeNew::EditSignal ()) <$) a)


tdButtonAttrs :: (Monad m, DomBuilder t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, MonadIO m)
  => Text -> a -> Map Text Text -> m (Event t a)
tdButtonAttrs s val attrs = do
  (element, _) <- elAttr' "td" attrs $ text s
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ ((val) <$) clickEv

-- with displayed text that can change
tdButtonAttrs' :: (Monad m, DomBuilder t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), Reflex t, PostBuild t m, MonadIO m)
  => Dynamic t Text -> a -> Map Text Text -> m (Event t a)
tdButtonAttrs' s val attrs = do
  (element, _) <- elAttr' "td" attrs $ dynText s
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ ((val) <$) clickEv

tdPingButtonAttrs :: (Monad m, DomBuilder t m, PostBuild t m) => Text -> Map Text Text -> a -> b -> m (Dynamic t ((),Event t (EditSignal ())))
tdPingButtonAttrs label attrs _ _ = el "td" $ do
  b <- buttonDynAttrs label (ChangeValue ()) $ constDyn attrs
  return $ constDyn ((), b)

growingTextInput :: MonadWidget t m => TextInputConfig t -> m (TextInput t)
growingTextInput config = mdo
  let attrs = _textInputConfig_attributes config
  let dynAttrs = (\m w-> insertWith (T.append) "style" (T.pack $ ";width:"++ show (max 20 $ min 100 $ 8*T.length w) ++ "px" ++";") m) <$> attrs <*> (_textInput_value textField)
  let newConfig = TextInputConfig (_textInputConfig_inputType config) (_textInputConfig_initialValue config) (_textInputConfig_setValue config) dynAttrs
  textField <- textInput newConfig
  return textField

whitespace :: (MonadWidget t m, Show a, Eq a) => Dynamic t Liveness -> GeneralPattern a -> Text -> [EditSignal (GeneralPattern a)] -> () -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t ((), Event t (EditSignal (GeneralPattern a)), Event t Hint))
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



whitespacePopup :: (MonadWidget t m, Show a) => Dynamic t Liveness -> [EditSignal a]  -> m (Event t (Maybe (EditSignal a)))
whitespacePopup liveness actionList = elClass "div" "popupMenu" $ do
  let popupList = fmap (\x->clickableDivClass' (T.pack $ show x) "primary-color code-font background" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  layerSplit <- clickableDivClass' "[ , ]" "primary-color code-font background" (LayerSplit)
  liveWidget <- livenessCheckboxWidget (liveness)
  closeMenu <- clickableDivClass' "close" "primary-color code-font background" (Nothing)
  return $ leftmost $ events' ++[closeMenu, fmap Just liveWidget, fmap Just layerSplit]


livenessWidget :: (Monad m, DomBuilder t m, Reflex t, MonadFix m, PostBuild t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), MonadHold t m, MonadIO m)
  => Dynamic t Liveness -> m (Event t (EditSignal a))
livenessWidget liveness = elClass "div" "livenessWidget" $ mdo
  let livenessText = fmap (\x->if x==L3 then "L3" else "L4") liveness
  livenessButton <- clickableDivClass'' (livenessText) "livenessText" ()
  eval <- clickableDivClass' "Eval" "L3Eval" Eval
  let livenessChange = attachWith (\d e -> if d==L4 then MakeL3 else MakeL4) (current liveness) livenessButton
  return $ leftmost [livenessChange,eval]

--(DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m, MonadHold t m)
livenessCheckboxWidget :: MonadWidget t m => Dynamic t Liveness -> m (Event t (EditSignal a))
livenessCheckboxWidget liveness = elClass "div" "livenessWidget" $ do
  text "Live"
  let isLive = fmap (==L4) liveness
  cb <- checkboxView (constDyn Data.Map.empty) isLive
  eval <- clickableDivClass' "Eval" "L3Eval" Eval
  return $ leftmost [fmap (\x-> if x then MakeL4 else MakeL3) cb,eval]

basicPopup :: (MonadWidget t m, Show a) => Dynamic t Liveness -> [EditSignal a]  -> m (Event t (Maybe (EditSignal a)))
basicPopup liveness actionList = elClass "div" "popupMenu" $ do
  let popupList = fmap (\x->clickableDivClass' (T.pack $ show x) "primary-color code-font background" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  liveWidget <- livenessCheckboxWidget (liveness)
  closeMenu <- clickableDivClass' "close" "primary-color code-font background" (Nothing)
  return $ leftmost $ events' ++[closeMenu, fmap Just liveWidget]

samplePickerPopup :: MonadWidget t m => Dynamic t Liveness -> Map Int (Text,Text) -> [EditSignal  Text] -> m (Event t (Maybe (EditSignal Text)),Event t Hint)
samplePickerPopup liveness sampleMap actionList  = elClass "div" "popupMenu" $ do
  dd <- dropdownOpts (-1) sampleMap def  --defaults to -1 so that someone can select "~" (the first one) and have it register as a change
  let sampleKey = _dropdown_value dd
  let sampleChange = fmap (\x-> maybe ("~") (snd) $ Data.Map.lookup x sampleMap) sampleKey -- Dyn (editsignal Text)
  let popupList = fmap (\x->clickableDivClass' (T.pack $ show x) "primary-color code-font background" (Just x)) actionList -- [m (Maybe (EditSignal))]
  let events = Control.Monad.sequence popupList  -- m (t a)
  events' <- liftM (id) events
  liveWidget <- livenessCheckboxWidget liveness
  closeMenu <- clickableDivClass' "close" "primary-color code-font background" (Nothing)
  return $ (leftmost $ events' ++[closeMenu, fmap Just liveWidget,fmap (Just . ChangeValue) (updated sampleChange)], fmap PreloadAudioBank $ ffilter (\x->if x =="~" then False else True) $ updated sampleChange)

repDivWidget' :: MonadWidget t m => RepOrDiv -> Event t () -> m (Event t RepOrDiv)
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

repDivWidget'' :: MonadWidget t m => RepOrDiv -> Event t () -> m (Dynamic t RepOrDiv)
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


genericSignalMenu :: (MonadFix m, DomBuilder t m, TriggerEvent t m, G.IsElement (RawElement (DomBuilderSpace m)), MonadIO m) => m (Event t (Maybe (EditSignal a)))
genericSignalMenu = elAttr "div" (singleton "style" "top: 0px; left: 0px; position: absolute; z-index: 1;") $ do
  a <- clickableDivClass' "Close" "primary-color code-font background" Nothing
  b <- clickableDivClass' "-" "primary-color code-font background" (Just DeleteMe)
  c <- clickableDivClass' "[]" "primary-color code-font background" (Just MakeGroup)
  d <- clickableDivClass' "{}" "primary-color code-font background" (Just MakeLayer)
  return $ leftmost [a,b,c,d]

popupSignalWidget :: (MonadFix m, Adjustable t m, DomBuilder t m, MonadHold t m, TriggerEvent t m, MonadIO m, G.IsElement (RawElement (DomBuilderSpace m))) => m (Event t (EditSignal a))
popupSignalWidget = elAttr "div" (singleton "style" "border: 1px solid black; position: relative; display: inline-block;") $ mdo
  y <- popup popupEvents
  x <- clickableWhiteSpace
  let popupEvents = leftmost [Just genericSignalMenu <$ x,Nothing <$ y]
  return $ (fmap fromJust . ffilter isJust) y

genericSignalWidget :: (Monad m, DomBuilder t m) => m (Event t (EditSignal a))
genericSignalWidget = elClass "div" "genericSignalWidget" $ do
  b <- button' "-" DeleteMe
  c <- button' "[]" MakeGroup
  d <- button' "{}" MakeLayer
  return $ leftmost [b,c,d]

-- Used heavily in Help section
-- Contains the child in a hideable div with a class
hideableWidget :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> m a -> m a
hideableWidget b c m = do
  let attrs = fmap (bool (fromList [("hidden","true"),("class",c)]) (singleton "class" c)) b
  elDynAttr "div" attrs m

-- Used for the terminal
-- Contains the child in a hideable div with no class
hideableWidget' :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m a
hideableWidget' b m = do
  let attrs = fmap (bool (fromList [("hidden","true")]) (fromList [("visible","true")])) b
  elDynAttr "div" attrs m

hideableWidgetWFlexColumn :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m a
hideableWidgetWFlexColumn b  m = do
  let attrs = fmap (bool (fromList [("hidden","true")]) (fromList [("style", "display: flex; flex-direction: column;")])) b
  elDynAttr "div" attrs m


traceDynamic :: (MonadIO m, Reflex t, MonadSample t m, MonadHold t m, Show a) => String -> Dynamic t a -> m (Dynamic t a)
traceDynamic msg x = do
  i <- sample $ current x
  liftIO $ putStrLn $ msg ++ ": " ++ show i
  let u = traceEvent msg (updated x)
  holdDyn i u

traceDynamicWith :: (MonadIO m, Reflex t, MonadSample t m, MonadHold t m) => (a -> String) -> Dynamic t a -> m (Dynamic t a)
traceDynamicWith f x = do
  i <- sample $ current x 
  let i' = f i
  liftIO $ putStrLn $ show i'
  let u = traceEventWith f (updated x)
  holdDyn i u

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

-- a tooltip with settable class for the popup
tooltipNoPopUpClass :: DomBuilder t m => m a -> m b -> m a
tooltipNoPopUpClass child popup = do
  elClass "div" "tooltip" $ do
    a <- child
    divClass "tooltipPosAbsolute" $ popup
    return a



-- below this line is the former Estuary.Reflex.Container


widgetMap :: (Monad m, Reflex t, Adjustable t m, MonadHold t m, Ord k) => Map k (m a) -> Event t (Map k (m a)) -> m (Dynamic t (Map k a))
widgetMap iMap rebuild = do
  let iWidget = sequence $ elems iMap -- :: m [a]
  let rebuild' = fmap (sequence . elems) rebuild -- :: Event t (m [a])
  widgets <- widgetHold iWidget rebuild' -- :: m (Dynamic t [a])
  keys <- holdDyn (keys iMap) (fmap keys rebuild) -- :: m (Dynamic t [k])
  return $ (\a b -> fromList $ zip a b) <$> keys <*> widgets

container' :: (Monad m, Reflex t, Adjustable t m, MonadHold t m, MonadFix m, Ord k, Num k)
  => (v -> m a) -- a builder function from
  -> Map k v -- an initial map of values
  -> Event t (Map k (Construction v)) -- construction events
  -> m (Dynamic t (Map k a))

container' build iMap cEvents = do
  let iMap' = fmap build iMap
  newMap <- foldDyn (\a b -> applyConstructionMap b a) iMap cEvents
  let newMap' = fmap (fmap build) (updated newMap)
  widgetMap iMap' newMap'


data Construction a = Insert a | Replace a | Delete deriving (Show)

-- given a Map and a Construction operation at a specifed key, return the new map

applyConstruction :: (Num k, Ord k) => Map k a -> (k,Construction a) -> Map k a
applyConstruction m (k,Replace a) = Data.Map.insert k a m
applyConstruction m (k,Delete) = Data.Map.delete k m
applyConstruction m (k,Insert a) = Data.Map.insert k a m'
  where m' = mapKeys (f) m
        f x | member k m = if (x>=k) then (x + 1) else x
            | otherwise = x

-- given a Map and another map of Construction operations, return the resulting map

applyConstructionMap :: (Num k, Ord k) => Map k a -> Map k (Construction a) -> Map k a
applyConstructionMap oldMap cMap = foldlWithKey (\a k b -> applyConstruction a (k,b)) oldMap cMap


-- given a Map and another map of Construction operations, determine the complete
-- list of "construction" events as expected by the Reflex function listHoldWithKey

constructionDiff :: (Num k, Ord k, Eq v) => Map k v -> Map k (Construction v) -> Map k (Maybe v)
constructionDiff oldMap cMap = unions [deletions,additions,changes]
  where newMap = applyConstructionMap oldMap cMap
        deletions = fmap (const Nothing) $ Data.Map.difference oldMap newMap -- keys only in oldMap are deletions
        additions = fmap (Just) $ Data.Map.difference newMap oldMap -- keys only in newMap are additions
        changes = fmap (Just) $ intersection newMap $ Data.Map.filter (id) $ intersectionWith (/=) oldMap newMap

container :: (MonadFix m, Reflex t, Adjustable t m, MonadHold t m, Ord k, Num k, Show k, Eq v, Show v)
   => Map k v                                -- a map of initial values
   -> Event t (Map k (Construction v))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                      -- signaling events to be delivered to child widgets
   -> (v -> Event t w -> m (Dynamic t (v,Event t x)))                -- function to make a widget given initial value and signaling event
   -> m ( (Dynamic t (Map k v)) , Event t (Map k x) )

container initialValue cEvents rEvents mkChild = mdo
  let cEventsIn = cEvents
  let existingMap' = values
  let cEvents' = attachPromptlyDynWith (constructionDiff) existingMap' cEventsIn
  let selector = fanMap rEvents
  let mkChild' k v = mkChild v $ select selector $ (Const2 k)
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValue cEvents' mkChild' -- Dynamic t (Map k (v,Event t x))
  let values = fmap (fmap (fst)) widgets
  let events = switchPromptlyDyn $ fmap (mergeMap . fmap (snd)) widgets
  return (values,events)


eitherContainer :: (MonadFix m, Reflex t, Monad m, Adjustable t m, MonadHold t m, Ord k, Num k, Show k, Eq v, Eq a)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e)))  -- function to build widgets for type v (returning events of type e)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e)))  -- function to build widgets for type a (also returning events of type e)
   -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) )

eitherContainer initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = mdo
  let cEvents' = attachPromptlyDynWith (constructionDiff) values cEvents
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValues cEvents' mkChild
  let values = fmap (fmap (fst)) widgets
  let events = switchPromptlyDyn $ fmap (mergeMap . fmap (snd)) widgets
  return (values,events)
  where
    mkChild k (Left x) = buildLeft x (select (fanMap eventsToLeft) (Const2 k)) >>= return . fmap (\(v,e)->(Left v,e))
    mkChild k (Right x) = buildRight x (select (fanMap eventsToRight) (Const2 k)) >>= return . fmap (\(a,e)->(Right a,e))


-- for widgets returning a 3rd event channel (for hints)
eitherContainer4 :: (MonadFix m, Reflex t, Monad m, Adjustable t m,MonadHold t m, Ord k, Num k, Show k, Eq v, Eq a)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e,Event t e1)))  -- function to build widgets for type v (returning events of type e)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e,Event t e1)))  -- function to build widgets for type a (also returning events of type e)
   -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) , Event t e1 )

eitherContainer4 initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = mdo
  let cEvents' = attachPromptlyDynWith (constructionDiff) values cEvents
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValues cEvents' mkChild -- m (Dynamic t (Map k a))
  let values = fmap (fmap (\(a,_,_)->a)) widgets
  let events = switchPromptlyDyn $ fmap (mergeMap . fmap ((\(_,b,_)->b))) widgets
  let events2 = switchPromptlyDyn $ fmap (leftmost . elems . fmap ((\(_,_,c)->c))) widgets -- @ may drop some messages if multiple hints coincide...
  return (values,events,events2)
  where
    mkChild k (Left x) = buildLeft x (select (fanMap eventsToLeft) (Const2 k)) >>= return . fmap (\(v,e,e2)->(Left v,e,e2))
    mkChild k (Right x) = buildRight x (select (fanMap eventsToRight) (Const2 k)) >>= return . fmap (\(a,e,e2)->(Right a,e,e2))


-- eitherContainer' is a variant of eitherContainer where the difference is that
-- only left values (not right) are included in the dynamic result:

eitherContainer' :: (Monad m, Adjustable t m, MonadFix m, MonadHold t m, Ord k, Num k, Show k, Eq v, Eq a)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))      -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e)))  -- function to build widgets for type v (returning events of type x)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e)))  -- function to build widgets for type a (returning events of type c)
   -> m ( (Dynamic t (Map k v)) , Event t (Map k e) )
eitherContainer' initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = do
  (d,e) <- eitherContainer initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight
  let d' = fmap (Data.Map.mapMaybe (either (Just) (const Nothing))) d
  return (d',e)


eitherContainer'' :: (MonadFix m, Reflex t, Adjustable t m, Monad m, MonadHold t m, Ord k, Num k, Show k, Eq v, Eq a)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e,Event t h)))  -- function to build widgets for type v (returning events of type e and f)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e,Event t h)))  -- function to build widgets for type a (also returning events of type e and f)
   -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e), Event t h)

eitherContainer'' initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = mdo
  let cEvents' = attachPromptlyDynWith (constructionDiff) values cEvents
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValues cEvents' mkChild
  let values = fmap (fmap (\(x,_,_) -> x)) widgets
  let eEvents = switchPromptlyDyn $ fmap (mergeMap . fmap (\(_,x,_) -> x)) widgets
  let hEvents = switchPromptlyDyn $ fmap (leftmost . elems . fmap (\(_,_,x) -> x)) widgets
  return (values,eEvents,hEvents)
  where
    mkChild k (Left x) = buildLeft x (select (fanMap eventsToLeft) (Const2 k)) >>= return . fmap (\(v,e,f)->(Left v,e,f))
    mkChild k (Right x) = buildRight x (select (fanMap eventsToRight) (Const2 k)) >>= return . fmap (\(a,e,f)->(Right a,e,f))

eitherContainer''' :: (Ord k, Num k, Show k, Eq v, Eq a, MonadHold t m, Adjustable t m, MonadFix m)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))      -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e,Event t h)))  -- function to build widgets for type v (returning events of type e and f)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e,Event t h)))  -- function to build widgets for type a (returning events of type e and f)
   -> m ( Dynamic t (Map k v) , Event t (Map k e), Event t h)
eitherContainer''' initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = do
  (d,e,h) <- eitherContainer'' initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight
  let d' = fmap (Data.Map.mapMaybe (either (Just) (const Nothing))) d
  return (d',e,h)


eitherWidget :: (Monad m, PostBuild t m)
  => (a -> Event t c -> m (Dynamic t (a,Event t d)))
  -> (b -> Event t c -> m (Dynamic t (b,Event t d)))
  -> Either a b -> Event t c -> m (Dynamic t ((Either a b),Event t d))

eitherWidget buildA buildB iValue c = either buildA' buildB' iValue
  where
    buildA' a = buildA a c >>= return . fmap (\(x,d) -> (Left x,d))
    buildB' b = buildB b c >>= return . fmap (\(x,d) -> (Right x,d))

wfor :: (Monad m, Reflex t, Adjustable t m, PostBuild t m, MonadHold t m) => [a] -> (a -> m (Dynamic t b)) -> m (Dynamic t [b])
wfor iVals mkChild = do
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey iMap never mkChild' -- m (Dynamic t [b]))
  return $ fmap elems widgets
  where
    iMap = fromList $ zip [(0::Int)..] iVals
    mkChild' _ a = mkChild a

wmap :: (Adjustable t m, PostBuild t m, MonadHold t m) => (a -> m (Dynamic t b)) -> [a] -> m (Dynamic t [b])
wmap = flip wfor


resettableWidget :: (Monad m, Adjustable t m, Reflex t, MonadHold t m) => (a -> Event t b -> m (Dynamic t c)) -> a -> Event t b -> Event t a -> m (Dynamic t c)
resettableWidget f i e reset = liftM (join) $ widgetHold (f i e) $ fmap (\x -> f x e) reset


popup :: (Monad m, Reflex t, Adjustable t m, MonadHold t m) => Event t (Maybe (m (Event t a))) -> m (Event t a)
popup buildEvents = do
  let buildEvents' = fmap (maybe (return never) id) buildEvents
  liftM (switchPromptlyDyn) $ widgetHold (return never) buildEvents'


clickableWhiteSpace :: (Monad m, Reflex t, TriggerEvent t m, DomBuilder t m, MonadIO m, G.IsElement (RawElement (DomBuilderSpace m))) => m (Event t ())
clickableWhiteSpace = do
  (element,_) <- elAttr' "div" (singleton "class" "clickableWhiteSpace") $ text "clickableWhiteSpace"
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ (() <$) clickEv


flippableWidget :: (Adjustable t m, Reflex t, MonadHold t m) => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)
flippableWidget b1 b2 i e = widgetHold (bool b1 b2 i) $ fmap (bool b1 b2) e


-- catchKeyboardShortcut: given a DOM element, a keyCode (Word), and Bools representing
-- whether the ctrl and shift keys should be pressed, prevent the default behaviour when
-- that keypress happens on that element, and fire an event.

catchKeyboardShortcut :: (Reflex t, TriggerEvent t m, G.MonadJSM m, G.IsHTMLElement e) => e -> Word -> Bool -> Bool -> m (Event t ())
catchKeyboardShortcut el keyCode ctrlKey shiftKey = do
  x <- wrapDomEvent el (onEventName Keypress) $ do
    e <- event
    k <- getKeyEvent
    c <- liftIO $ _getCtrlKey (G.unKeyboardEvent e)
    s <- liftIO $ _getShiftKey (G.unKeyboardEvent e)
    if (k == keyCode && c == ctrlKey && s == shiftKey) then
      (GHCJS.DOM.EventM.preventDefault >> return (Just ()))
      else return Nothing
  return $ fmapMaybe id x

foreign import javascript unsafe "$1['ctrlKey']"
  _getCtrlKey :: T.JSVal -> IO Bool

foreign import javascript unsafe "$1['shiftKey']"
  _getShiftKey :: T.JSVal -> IO Bool
