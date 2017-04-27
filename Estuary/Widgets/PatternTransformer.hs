module Estuary.Widgets.PatternTransformer where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Text.Read
import GHC.Real


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
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"]))
  let input' = _textInput_value input -- Dyn string
  input2 <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ ("1")
  let input2' = _textInput_value input2 -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Integer))
  val2 <- forDyn input2' (\x-> maybe 1 id (readMaybe x::Maybe Integer))
  combineDyn (\x y->  Slow ((x%y)::Rational)) val val2
paramWidget (Density _)= do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"]))
  let input' = _textInput_value input -- Dyn string
  input2 <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ ("1")
  let input2' = _textInput_value input2 -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Integer))
  val2 <- forDyn input2' (\x-> maybe 1 id (readMaybe x::Maybe Integer))
  combineDyn (\x y-> Density $ (x%y::Rational) ) val val2
paramWidget (DegradeBy _) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Double))
  val'<-forDyn val (\k-> DegradeBy k)
  return val'
paramWidget (Chop _) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number"))
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Int))
  val'<-forDyn val (\k-> Chop k)
  return val'
paramWidget transformer = return $ constDyn transformer

parameteredPatternTransformer::MonadWidget t m => PatternTransformer -> Event t () -> m (Dynamic t (PatternTransformer, ()))
parameteredPatternTransformer i _ = el "div" $ do
  let transMap = fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9] [NoTransformer,Rev,Slow 1, Density 1, Degrade, DegradeBy 0.5, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1]
  let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9] ["NoTransformer","Rev","Slow","Density", "Degrade", "DegradeBy","Brak","Every","Jux","Chop"]
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
    hack (Chop _) = 9
    hack _ = 0
