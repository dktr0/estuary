module Estuary.Reflex.Utility where

import Reflex
import Reflex.Dom
import Control.Monad (liftM)
import Data.Map
import GHCJS.DOM.EventM
import Data.Map
import Data.Maybe
import Data.Monoid
import GHCJS.DOM.HTMLSelectElement as Select
import Control.Monad hiding (forM_) -- for 'guard'
import Safe -- for readMay
import GHCJS.DOM.Element hiding (error) --for 'change'
import Data.List (nub, elemIndex)

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


-- dropdownOpts :: (MonadWidget t m, Ord k, Show k, Read k) => k -> Dynamic t (Map k (String,String)) ->  DropdownConfig t k -> m (Dropdown t k)
-- dropdownOpts k0 setUpMap (DropdownConfig setK attrs) = do
--   options <- forDyn setUpMap (\k-> fromList $ zip (keys k) (fmap snd $ elems k))
--   optGroups <- forDyn setUpMap (\k-> fromList $ zip (keys k) (fmap fst $ elems k))
--   (eRaw, _) <- elDynAttr' "select" attrs $ do
--     optionsWithDefault <- mapDyn (`Data.Map.union` (k0 =: "")) options
--     listWithKey optionsWithDefault $ \k v -> do
--       a <- forDyn optGroups (\groupMap-> elAttr "optgroup" ("label"=: (maybe "" show $ Data.Map.lookup k groupMap)))
--       elAttr "option" ("value" =: show k <> if k == k0 then "selected" =: "selected" else mempty) $ dynText v
--   let e = castToHTMLSelectElement $ _el_element eRaw
--   performEvent_ $ fmap (Select.setValue e . Just . show) setK
--   eChange <- wrapDomEvent e (`on` change) $ do
--     kStr <- fromMaybe "" <$> Select.getValue e
--     return $ readMay kStr
--   let readKey opts mk = fromMaybe k0 $ do
--         k <- mk
--         guard $ Data.Map.member k opts
--         return k
--   dValue <- combineDyn readKey options =<< holdDyn (Just k0) (leftmost [eChange, fmap Just setK])
--   return $ Dropdown dValue (attachDynWith readKey options eChange)


dropdownOpts :: (MonadWidget t m) => Int -> Map Int (String,String) ->  DropdownConfig t Int -> m (Dropdown t Int)
dropdownOpts k0 setUpMap (DropdownConfig setK attrs) = do
  let options = fromList $ zip (keys setUpMap) $ fmap snd $ elems setUpMap
  let optGroups = fromList $ zip (keys setUpMap) $ fmap fst $ elems setUpMap
  let optGroupPositions = fmap (\x-> maybe (0) id $ Data.List.elemIndex x (elems optGroups)) $ nub $ elems optGroups -- [Int]
  (eRaw, _) <- elDynAttr' "select" attrs $ do
    let optionsWithDefault = constDyn $ if Data.Map.lookup k0 options == Nothing then Data.Map.union (k0 =: "") options else options
    listWithKey optionsWithDefault $ \k v -> do
      if not (elem k optGroupPositions) then blank else do
        elAttr "optgroup" ("label"=:(maybe "" id $ Data.Map.lookup k optGroups)) $ blank
      elAttr "option" ("value" =: show k <> if k == k0 then "selected" =: "selected" else mempty) $ dynText v
  let e = castToHTMLSelectElement $ _el_element eRaw
  performEvent_ $ fmap (Select.setValue e . Just . show) setK
  eChange <- wrapDomEvent e (`on` change) $ do
    kStr <- fromMaybe "" <$> Select.getValue e
    return $ readMay kStr
  let readKey mk = fromMaybe k0 $ do
        k <- mk
        guard $ Data.Map.member k options
        return k
  dValue <- mapDyn readKey =<< holdDyn (Just k0) (leftmost [eChange, fmap Just setK])
  return $ Dropdown dValue (fmap readKey eChange) -- @clean this.

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
