module Estuary.Widgets.PatternTransformer where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Text.Read

trivialPatternTransformer :: MonadWidget t m => m (Dynamic t (PatternTransformer,()))
trivialPatternTransformer = el "div" $ do
  let ddItems = [NoTransformer,Rev,Slow 2,Density 2,Degrade,DegradeBy 0.9,Brak]
  let ddShow = Prelude.map (show) ddItems
  let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6] ddShow
  dd <- dropdown 0 ddMap def
  dd' <- mapDyn (ddItems!!) $ _dropdown_value dd
  dd'' <- mapDyn (ddShow!!) $ _dropdown_value dd
  display dd''
  mapDyn (\a -> (a,())) $ dd'

-- NoTransformer | Rev | Slow Rational | Density Rational | Degrade | DegradeBy Double | Every Int PatternTransformer | Brak deriving (Ord,Eq)
-- parameteredPatternTransformer::MonadWidget t m => m(Dynamic t (PatternTransformer, ()))
-- parameteredPatternTransformer = el "div" $ do
--   let disable = "disabled"=:"disabled"
--   let num = "type"=:"number"
--   let ddItems = ["NoTransformer","Rev","Slow","Density", "Degrade","DegradeBy","Brak"]
--   let paramOneAttrsMap = fromList $ zip [0..] [disable,disable,num,num,disable,num,disable]  -- map transformer (Map String String) --second map are the attrs
--   let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6] ddItems
--   dd <- dropdown 0 ddMap def
--   ddVal<- mapDyn (\a->read ddItems!!a::) $ _dropdown_value dd -- Dynamic t PatternTransformer
--   paramOneAttrs <- mapDyn (\x-> case Data.Map.lookup x paramOneAttrsMap of Just a->a; otherwise->empty) dd
--   --paramCategory <- forDyn paramOneAttrs (\x-> case x of disable)
--   paramOneField <- textInput $ def & textInputConfig_attributes .~ paramOneAttrs
--   let paramOne = _textInput_value paramOneField
--   transformer <- combineDyn f ddVal paramOne
--   mapDyn (\a -> (a,())) $ transformer
--   where
--     f trans param
--       | elem param noParams = trans
--       | elem param oneParam = trans (read param::Int)
--       | otherwise = trans
--       where
--         (noParams,oneParam) = ([NoTransformer,Rev,Degrade,Brak],[Slow,Density,DegradeBy])



paramWidget::MonadWidget t m=>PatternTransformer -> m (Dynamic t PatternTransformer)
paramWidget NoTransformer = return $ constDyn NoTransformer
paramWidget Rev = return $ constDyn Rev
paramWidget Degrade = return $ constDyn Degrade
paramWidget Brak = return $ constDyn Brak
paramWidget (Slow _) = el "div" $ do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Rational))
  val'<-forDyn val (\k-> Slow (k))
  display val
  return val'
paramWidget (Density _)= el "div" $ do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Rational))
  val'<-forDyn val (\k-> Density k)
  return val'
paramWidget (DegradeBy _) = el "div" $ do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Double))
  val'<-forDyn val (\k-> DegradeBy k)
  return val'

parameteredPatternTransformer::MonadWidget t m => m(Dynamic t (PatternTransformer, ()))
parameteredPatternTransformer = el "div" $ do
  let transMap = fromList $ zip [0::Int,1,2,3,4,5,6] [NoTransformer,Rev,Slow 1, Density 1, Degrade, DegradeBy 0.5, Brak]
  let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6] ["NoTransformer","Rev","Slow","Density", "Degrade", "DegradeBy","Brak"]
  dd <- dropdown 0 ddMap def
  let ddVal = _dropdown_value dd -- Dynamic int
  ddWidget <- mapDyn (\k ->case Data.Map.lookup k transMap of Just a->paramWidget a; otherwise -> paramWidget NoTransformer) ddVal  --Dynamic (m(dynamic transformer))
  let ddWidgetValEv =  updated ddWidget -- Event (M (dyn Pattern))
  paramValue <- widgetHold (paramWidget NoTransformer) ddWidgetValEv  -- m Dynamic t(m (Dynamic PatternTrans...))
  let paramValue' = joinDyn paramValue --Dyn PatternTransformer
  mapDyn (\k->(k,())) paramValue'
