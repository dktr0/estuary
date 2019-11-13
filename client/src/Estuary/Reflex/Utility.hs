{-# LANGUAGE OverloadedStrings #-}

module Estuary.Reflex.Utility where

import Reflex
import Reflex.Dom
import Data.Map
import Data.Text as T
import GHCJS.DOM.EventM
import qualified GHCJS.DOM.Types as G
import Data.Map
import Data.Maybe
import Data.Monoid
import Control.Monad
import GHCJS.DOM.HTMLSelectElement as Select
import Safe -- for readMay

import GHCJS.DOM.GlobalEventHandlers (change)
import Data.List (nub, elemIndex)

import Estuary.Types.Term
import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Types.TranslatableText

translateDyn :: MonadWidget t m => Term -> Dynamic t Context -> m (Dynamic t Text)
translateDyn t ctx = do
  l <- holdUniqDyn $ fmap language ctx
  return $ fmap (translate t) l

-- translationList should be considered deprecated in favour of TranslationText etc
translationList :: MonadWidget t m => Dynamic t Context -> [(Language,a)] -> m (Dynamic t a)
translationList ctx m = do
  let m' = fromList m
  let d = snd (m!!0)
  l <- holdUniqDyn $ fmap language ctx
  return $ fmap (\k -> findWithDefault d k m') l

-- a temporary button with class for the reference files
buttonWithClass' :: MonadWidget t m => Text -> m (Event t ())
buttonWithClass' s = do
  (e, _) <- elAttr' "button" (fromList [("type", "button"), ("class", "ui-buttons code-font primary-color"), ("style", "background-color:transparent; border:none; cursor:help")]) $ text s
  return $ domEvent Click e

-- a button with class
buttonWithClass :: MonadWidget t m => Text -> m (Event t ())
buttonWithClass s = do
  (e, _) <- elAttr' "button" (fromList [("type", "button"), ("class", "ui-buttons other-borders code-font")]) $ text s
  return $ domEvent Click e

--Button with dynamic label.
dynButton :: MonadWidget t m => Dynamic t Text -> m (Event t ())
dynButton = dynE . fmap buttonWithClass

dynButtonWithChild :: MonadWidget t m => String -> m () -> m (Event t ())
dynButtonWithChild cls child = do
  (e, _) <- elAttr' "div" (fromList [("type", "button"), ("class", T.pack $ cls ++ " btn")]) child
  return $ domEvent Click e

-- | dynE is like dyn from Reflex, specialized for widgets that return
-- events. A dynamic argument updates the widget, and the return value is
-- already flattened to just being the events returned by the child widget.
dynE :: MonadWidget t m => Dynamic t (m (Event t a)) -> m (Event t a)
dynE x = dyn x >>= switchPromptly never

-- a button that, instead of producing Event t (), produces an event of
-- some constant value
button' :: (MonadWidget t m) => Text -> a -> m (Event t a)
button' t r = do
  x <- button t
  return (r <$ x)

-- Button With Dynamic attributes
buttonDynAttrs :: MonadWidget t m => Text -> a -> Dynamic t (Map Text Text)-> m (Event t a)
buttonDynAttrs s val attrs = do
  (e, _) <- elDynAttr' "button" attrs $ text s
  let event = domEvent Click e
  return (val <$ event)

-- Creates dropdown Menu with Subheaders
-- takes a Map of integers (the order everything should be displayed in)
-- to String tuples. The first String of the tuple indicates a subheader,
-- and the second indicates the selectable item under it. DropdownConfig options
-- expect the same as with a regular dropdown
dropdownOpts :: (MonadWidget t m) => Int -> Map Int (Text,Text) ->  DropdownConfig t Int -> m (Dropdown t Int)
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
  let e = G.uncheckedCastTo HTMLSelectElement $ _el_element eRaw
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
