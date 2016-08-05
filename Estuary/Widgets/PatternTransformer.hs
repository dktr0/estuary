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
paramWidget (Jux trans) = do
  trans' <- parameteredPatternTransformer trans never
  val'<- mapDyn (\(next,_)-> Jux next) trans'
  return val'
paramWidget (Every num trans) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Int))
  nextTrans <- parameteredPatternTransformer trans never
  val'<-combineDyn (\k (next,_)-> Every k next) val nextTrans
  return val'
paramWidget (Slow _) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Rational))
  val'<-forDyn val (\k-> Slow (k))
  return val'
paramWidget (Density _)= do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Rational))
  val'<-forDyn val (\k-> Density k)
  return val'
paramWidget (DegradeBy _) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Double))
  val'<-forDyn val (\k-> DegradeBy k)
  return val'
paramWidget transformer = return $ constDyn transformer

parameteredPatternTransformer::MonadWidget t m => PatternTransformer -> Event t () -> m(Dynamic t (PatternTransformer, ()))
parameteredPatternTransformer i _ = el "div" $ do
  let transMap = fromList $ zip [0::Int,1,2,3,4,5,6,7,8] [NoTransformer,Rev,Slow 1, Density 1, Degrade, DegradeBy 0.5, Brak,Every 1 NoTransformer, Jux NoTransformer]
  let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6,7,8] ["NoTransformer","Rev","Slow","Density", "Degrade", "DegradeBy","Brak","Every","Jux"]
  dd <- dropdown (hack i) ddMap def
  let ddVal = _dropdown_value dd -- Dynamic int
  ddWidget <- mapDyn (\k ->case Data.Map.lookup k transMap of Just a->paramWidget a; otherwise -> paramWidget NoTransformer) ddVal  --Dynamic (m(dynamic transformer))
  let ddWidgetValEv =  updated ddWidget -- Event (M (dyn Pattern))
  paramValue <- widgetHold (paramWidget i) ddWidgetValEv  -- m Dynamic t(m (Dynamic PatternTrans...))
  let paramValue' = joinDyn paramValue --Dyn PatternTransformer
  mapDyn (\k->(k,())) paramValue'
  where
    hack NoTransformer = 0
    hack Rev = 1
    hack (Slow _) = 2 -- sorry...
    hack (Density _) = 3
    hack Degrade = 4
    hack (DegradeBy _) = 5
    hack Brak = 6
    hack (Every _ _)= 7
    hack (Jux _) = 8
    hack _ = 0
