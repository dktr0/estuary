{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.Roulette where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Data.Time
import GHCJS.DOM.EventM

-- import Data.Tuple.Select
import Control.Monad.Trans (liftIO)
import Data.Map.Strict
import Control.Monad
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleRequest
import Estuary.Types.Participant
import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Types.Definition
import Estuary.Widgets.Text
import Control.Monad

import qualified Estuary.Types.Term as Term

getHead :: [Text] -> [Text]
getHead [] = []
getHead xs = [head xs]


rouletteWidget :: MonadWidget t m => Int -> Dynamic t Roulette -> W t m (Variable t Roulette)
rouletteWidget rows delta = do
  let rows' = 1 + rows
  let attrsRouletteWidgetContainer = case rows of 0 -> constDyn $ ("class" =: "rouletteWidgetContainer  primary-color code-font")
                                                  _ -> constDyn $ ("class" =: "rouletteWidgetContainer  primary-color code-font" <> "style" =: ("height: " <> (T.pack (show rows') <> "em;"))) --  <> (wrappingRoulette wrappingBool)))

  let attrsRouletteContainer = case rows of 0 -> constDyn $ ("class" =: "rouletteContainer"  <> "style" =: "flex-wrap: wrap;")
                                            _ -> constDyn $ ("class" =: "rouletteContainer" <> "style" =: "flex-wrap: wrap;")

  elDynAttr "div" attrsRouletteWidgetContainer $ do
      elDynAttr "div" attrsRouletteContainer $ rouletteWidget' delta

rouletteWidget' :: MonadWidget t m => Dynamic t Roulette -> W t m (Variable t Roulette)
rouletteWidget' delta = mdo
    ctx <- context
    uHandle <- sample $ current $ fmap (userHandle . ensembleC) ctx -- Text

    let currentValHead = liftM getHead currentVal
    let currentValTail =  liftM (Prelude.drop 1) currentVal -- [Text] or Roulette [1, 2, 3] => [2, 3]
    let currentValUnion =  liftM2 (++) currentValHead  currentValTail

    -- roulette buttons/ not on a different colour, underlining or circling or subtle. Inverse colors, inverse the background and the font.
    listOfRouletteButtonsHead <- simpleList currentValHead (rouletteButton $ attrsRouletteButtonHead uHandle)  -- m (Dynamic [Event t (Roulette -> Roulette)])
    listOfRouletteButtonsTail <- simpleList currentValTail (rouletteButton $ attrsRouletteButtonTail uHandle) -- m (Dynamic [Event t (Roulette -> Roulette)])
    let listOfRouletteButtons = liftM2 (++) listOfRouletteButtonsHead listOfRouletteButtonsTail
    let deleteEv = switchDyn $ fmap leftmost listOfRouletteButtons

    -- lineup button
    let dynAttrs = attrsLineUpButton <$> fmap (currentlyLinedUp uHandle) currentVal
    lineupEv <- lineUpButton dynAttrs "+" (addHandleToList uHandle) -- (Event t (Roulette -> Roulette)

    -- let currentVal = constDyn ["luis", "jessica", "jamie"]-- <- holdUniqDyn $ currentValue x -- Dynamic t [Text]
    let editEvs = mergeWith (.) [deleteEv,lineupEv] -- Event a
    let newValue = attachWith (flip ($)) (current currentVal) editEvs --Event c
    x <- returnVariable delta newValue
    currentVal <- holdUniqDyn $ currentValue x
    return x

-- Dynamic t a (comes from the server, and only is the initial value and it also represents the udpates) -> Event t a (come from local user actions) -> Variable t Roulette
currentlyLinedUp :: Text -> [Text] -> Bool
currentlyLinedUp uHandle roulette
  |elem uHandle roulette = True
  |uHandle == "" = True
  |otherwise = False

rouletteButton :: MonadWidget t m => Map Text Text -> Dynamic t Text -> m (Event t (Roulette -> Roulette))
rouletteButton attrs label = do
  (element, _) <- elAttr' "div" attrs $ dynText $ label <> (constDyn "ⓧ")
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  return $ attachWith (\x _ -> removeHandleFromList x) (current label) clickEv


-- attrsRouletteButton :: Text -> Map Text Text
-- attrsRouletteButton uhandle
--   | uhandle == "" = "class" =: "rouletteButtons ui-buttons code-font" <> "style" =: "cursor: not-allowed; pointer-events: none;"
--   | otherwise = "class" =: "rouletteButtons ui-buttons code-font"

attrsRouletteButtonHead :: Text -> Map Text Text
attrsRouletteButtonHead uhandle
  | uhandle == "" = "class" =: "rouletteButtons ui-buttons code-font attrsRouletteButtonHead" <> "style" =: "cursor: not-allowed; pointer-events: none;"
  | otherwise = "class" =: "rouletteButtons ui-buttons code-font attrsRouletteButtonHead"


attrsRouletteButtonTail :: Text ->  Map Text Text
attrsRouletteButtonTail uhandle
  | uhandle == "" = "class" =: "rouletteButtons ui-buttons code-font attrsRouletteButtonTail" <> "style" =: "cursor: not-allowed; pointer-events: none;"
  | otherwise = "class" =: "rouletteButtons ui-buttons code-font attrsRouletteButtonTail"


lineUpButton ::  MonadWidget t m => Dynamic t (Map Text Text) -> Text -> (Roulette -> Roulette) -> m (Event t (Roulette -> Roulette))
lineUpButton attrs label r = do
  (element, _) <- elDynAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_element_raw element) (elementOnEventName Click) (mouseXY)
  let roulette = r <$ clickEv
  return roulette

attrsLineUpButton :: Bool -> Map Text Text
attrsLineUpButton b = "class" =: "lineUpButton ui-buttons other-borders code-font" <> "style" =: (pevents b <> cursor b <> colour b)
  where
    pevents False  = "pointer-events: auto; "
    pevents True = "pointer-events: none; "
    cursor False = "cursor: pointer; "
    cursor True = "cursor: not-allowed; "
    colour False =  "color:var(--primary-color)"
    colour True =  "color: var(--secondary-color)"

rouletteToRoulette :: Dynamic t Roulette -> Dynamic t Roulette
rouletteToRoulette xs = xs


addHandleToList :: Text -> Roulette -> Roulette
addHandleToList uHandle roulette
  |elem uHandle roulette = roulette
  |otherwise = (++) roulette [uHandle]
--addHandleToList "luis" -- Roulette -> Roulette -- check if the name is in the list already and add if not.
-- Both of these func should be pure funcs.

-- e.g. removeHandleFromList :: Text -> Roulette -> Roulette
-- deleteHandlremoveHandleFromListeFromL -- fail silently , i.e. return the existing list
removeHandleFromList :: Text -> Roulette -> Roulette
removeHandleFromList handle xs = Prelude.filter (\e -> e/=handle) xs
