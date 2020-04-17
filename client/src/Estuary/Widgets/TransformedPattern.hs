{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.TransformedPattern where

import Reflex
import Reflex.Dom hiding (Subtract,End)
import Control.Monad
import Data.Map
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Real
import Data.Maybe (fromJust)
import Text.Read

import Estuary.Tidal.Types
import Estuary.WebDirt.Foreign
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Reflex.Container
import qualified Estuary.Widgets.SpecificPattern as Sp
import Estuary.Utility (lastOrNothing)
import Estuary.Types.Hint
import Estuary.Types.Variable
import Estuary.Widgets.Editor

structureEditor :: MonadWidget t m =>
  Dynamic t TransformedPattern -> Editor t m (Variable t TransformedPattern)
structureEditor x = variableWidget x topLevelTransformedPatternWidget


topLevelTransformedPatternWidget :: MonadWidget t m
  => TransformedPattern
  -> Event t TransformedPattern
  -> Editor t m (Event t TransformedPattern)
topLevelTransformedPatternWidget i delta = do
  let updates = fmap midLevelTransformedPatternWidget delta
  w <- widgetHold (midLevelTransformedPatternWidget i) updates
  let edits = switchPromptlyDyn $ fmap fst w
  hints $ switchPromptlyDyn $ fmap snd w
  return edits


midLevelTransformedPatternWidget:: MonadWidget t m =>
  TransformedPattern -> m (Event t TransformedPattern, Event t [Hint])
midLevelTransformedPatternWidget iTransPat = do
  tuple <- resettableTransformedPatternWidget iTransPat never
  let dynVal = fmap (\(x,_,_)->x) tuple
  -- let editEv = tagPromptlyDyn dynVal $ switchPromptlyDyn $ fmap (\(_,x,_)->x) tuple -- :: EditSignal a
  let editEv = updated dynVal -- ** BROKEN: in ensemble mode this will lead to infinite request-response cycles in ensembles where nClients > 1
  -- structure editor needs to be reworked around Editor monad
  -- until then, we will just remove it from default views
  let hs = fmap (:[]) $ switchPromptlyDyn $ fmap (\(_,_,x)->x) tuple
  return (editEv,hs)


popupSpecificPatternWidget :: (MonadWidget t m)=> SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (EditSignal a),Event t Hint))
popupSpecificPatternWidget iValue _ = elClass "div" "patternTransformerWidget-or-popupSpecificPatternWidget" $ mdo
  let popup = specificPatternPopup [DeleteMe, TransformMe]
  openEv <- clickableSpanClass showSpecPat "primary-color code-font background" ()
  dynPopup <- liftM switchPromptlyDyn $ flippableWidget (return never) popup False $ updated dynOpen
  dynOpen <- toggle False $ leftmost [openEv,(() <$ )dynPopup]
  let changeEvents = fmap ((\x->case x of ChangeValue a->a;otherwise->(S $ Blank Inert)) . fromJust) $ ffilter (\x->case x of Just (ChangeValue a) ->True;otherwise->False) dynPopup
  let deleteEvent = fmap (\x->case x of Just DeleteMe -> DeleteMe; otherwise->Close) $ ffilter (\x-> case x of Just (DeleteMe)->True;otherwise->False) dynPopup
  let transformEvent = fmap (\x->case x of Just TransformMe -> TransformMe; otherwise->Close) $ ffilter (\x-> case x of Just (TransformMe)->True; otherwise->False) dynPopup
  specPatTuple <- liftM join $ widgetHold (Sp.specificContainer iValue never) $ fmap (flip Sp.specificContainer $ never) changeEvents
  let specPat = fmap (\(x,_,_)->x) specPatTuple
  let showSpecPat = fmap hack specPat
  return $ fmap (\(x,y,z)-> (x,leftmost [transformEvent,deleteEvent],z)) specPatTuple
  where
    hack (Accelerate _) = " accelerate "
    hack (Bandf _) = " bandf "
    hack (Bandq _) = " bandq " -- sorry...
    hack (Begin _) = " begin "
    hack (Coarse _) = " coarse "
    hack (Crush _) = " crush "
    hack (Estuary.Tidal.Types.Cut _) = " cut "
    hack (Cutoff _) = " cutoff "
    hack (Delay _) = " delay "
    hack (Delaytime _) = " delaytime "
    hack (Delayfeedback _) = " delayfeedback "
    hack (End _) = " end "
    hack (Gain _) = " gain"
    hack (Hcutoff _) = " hcutoff"
    hack (Hresonance _) = " hresonance"
    hack (Loop _) = " loop "
    hack (N _) =  " n "
    hack (Pan _) = " pan "
    hack (Resonance _) = " resonance "
    hack (S _) = " s "
    hack (Shape _) = " shape "
    hack (Sound _) = " sound "
    hack (Speed _) = " speed "
    hack (Unit _) = " unit "
    hack (Up _) = " up "
    hack (Vowel _) = " vowel "


specificPatternPopup:: MonadWidget t m => [EditSignal SpecificPattern] -> m (Event t (Maybe (EditSignal SpecificPattern)))
specificPatternPopup actionList = elClass "div" "popupMenu" $ do
  let specMap = fromList $ zip [(0::Int)..] iValueList
  let ddMap = constDyn $ fromList $ zip [(0::Int)..] ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"] -- Map (Map k func) String
  dd <- dropdown (-1) ddMap (def & attributes .~ constDyn ("class" =: "code-font background primary-color primary-borders"))
  let specPatKey = _dropdown_value dd
  let specChange = fmap (Just . ChangeValue . maybe (S $ Blank Inert) id . (flip Data.Map.lookup) specMap) specPatKey
  let popupList = fmap (\x->clickableDivClass' (T.pack $ show x) "primary-color code-font background" (Just x)) actionList -- [m (Maybe (EditSignal))]
  edits <- liftM id $ Control.Monad.sequence popupList
  closeMenu <- clickableDivClass' "close" "primary-color code-font background" (Nothing)
  return $ (leftmost $ edits ++[closeMenu,updated specChange])
  where
    iValueList = [
      (Accelerate $ Atom 0 Inert Once),
      (Bandf $ Atom 440 Inert Once),
      (Bandq $ Atom 10 Inert Once),
      (Begin $ Atom 0 Inert Once),
      (Coarse $ Atom 0 Inert Once),
      (Crush $ Atom 16 Inert Once),
      (Estuary.Tidal.Types.Cut $ Atom 1 Inert Once),
      (Cutoff $ Atom 440 Inert Once),
      (Delay $ Atom 0 Inert Once),
      (Delayfeedback $ Atom 0 Inert Once),
      (Delaytime $ Atom 0.5 Inert Once),
      (End $ Atom 1 Inert Once),
      (Gain $ Atom 1 Inert Once),
      (Hcutoff $ Atom 440 Inert Once),
      (Hresonance $ Atom 20 Inert Once),
      (Loop $ Atom 0 Inert Once),
      (N $ Atom 0 Inert Once),
      (Pan $ Atom 0.5 Inert Once),
      (Resonance $ Atom 0.5 Inert Once),
      (S $ Atom "~" Inert Once),
      (Shape $ Atom 0.5 Inert Once),
      (Speed $ Atom 1 Inert Once),
      (Unit $ Atom 'c' Inert Once),
      (Up $ Atom 0 Inert Once),
      (Vowel $ Atom 'o' Inert Once)]


patternCombinatorDropDown :: MonadWidget t m => PatternCombinator -> Event t () -> m (Dynamic t (PatternCombinator,Event t (EditSignal a)))
patternCombinatorDropDown iValue _ = do
  let ddMapVals = fromList $ zip [(1::Int)..] [Merge,Add,Subtract,Multiply,Divide]
  let ddMap = constDyn $ fromList $ zip [(1::Int)..] $ fmap (T.pack . show) [Merge,Add,Subtract,Multiply,Divide]
  dd <- dropdown iIndex ddMap def
  let choice = _dropdown_value dd
  let val = fmap (maybe Merge id . (flip Data.Map.lookup) ddMapVals) choice
  return $ fmap (\x->(x,never)) val
  where
    iIndex = case iValue of
      (Merge) -> 1
      (Add) -> 2
      (Subtract) -> 3
      (Multiply) -> 4
      (Divide) -> 5
      -- sorry....

patternCombinatorDropDown' :: MonadWidget t m => PatternCombinator -> Event t () -> m (Dynamic t (PatternCombinator,Event t (EditSignal a)))
patternCombinatorDropDown' _ _ = do
  return $ constDyn (Merge,never)


resettableTransformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t (EditSignal a) -> m (Dynamic t(TransformedPattern, Event t (EditSignal a),Event t Hint))
resettableTransformedPatternWidget iTransPat ev = mdo
  val <- resettableWidget (transformedPat) iTransPat ev resetEvent
  let tPat = fmap (\(x,_,_)->x) val
  let e = switchPromptlyDyn $ fmap (\(_,x,_)->x) val
  let h = switchPromptlyDyn $ fmap (\(_,_,x)->x) val
  let resetEvent = tagPromptlyDyn tPat $ ffilter (\x-> case x of RebuildMe->True; DeleteMe -> True; otherwise -> False) e
  return $ fmap (\x->(x,e,h)) tPat


transformedPat :: MonadWidget t m => TransformedPattern -> Event t (EditSignal a) -> m (Dynamic t (TransformedPattern, Event t (EditSignal a), Event t Hint))
transformedPat (EmptyTransformedPattern) _ = do
  x <- liftM (UntransformedPattern (S (Atom "~" Inert Once))  <$) $ dynButton "+"
  value <- holdDyn EmptyTransformedPattern x
  let event = (RebuildMe <$) x
  return $ fmap (\y -> (y,event,never)) value
transformedPat (UntransformedPattern specificPattern) _= do
  sPatTuple <- popupSpecificPatternWidget specificPattern never
  let sPat = fmap (\(x,_,_)->x) sPatTuple
  let sEv = switchPromptlyDyn $ fmap (\(_,x,_)->x) sPatTuple
  let delete = ffilter (\x->case x of DeleteMe->True;otherwise->False) sEv
  let transform = ffilter (\x->case x of TransformMe->True;otherwise->False) sEv
  let hint = switchPromptlyDyn $ fmap (\(_,_,h)->h) sPatTuple
  let tPat = fmap UntransformedPattern sPat
  combine <- el "div" $ dynButton "+"
  let delete' = (EmptyTransformedPattern <$) delete
  let updatedValue = attachPromptlyDynWith (\sp _-> constDyn $ TransformedPattern NoTransformer sp) tPat transform
  let combineValue = attachPromptlyDynWith (\sp trans-> constDyn $ TransformedPattern (Combine sp Merge) $ UntransformedPattern (Speed $ Atom  1 Inert Once)) sPat combine
  let updatedValue' = leftmost [updatedValue, combineValue, fmap constDyn delete']
  value <- liftM join $ holdDyn tPat updatedValue'
  let rebuildEvents = leftmost [(DeleteMe <$) delete, (RebuildMe <$) transform, (RebuildMe <$) combine]
  return $ fmap (\x->(x,rebuildEvents,hint)) value
transformedPat (TransformedPattern (Combine iSpecPat iPatComb) EmptyTransformedPattern) _ = transformedPat (UntransformedPattern iSpecPat) never
transformedPat (TransformedPattern (Combine iSpecPat iPatComb) iTransPat) _ = do
  sPatTuple <- popupSpecificPatternWidget iSpecPat never
  let sPat = fmap (\(x,_,_)->x) sPatTuple
  let sHint = switchPromptlyDyn $ fmap (\(_,_,h)->h) sPatTuple
  let sEv = switchPromptlyDyn $ fmap (\(_,x,_)->x) sPatTuple
  let delete = ffilter (\x->case x of DeleteMe->True;otherwise->False) sEv
  let transform = ffilter (\x->case x of TransformMe->True;otherwise->False) sEv
  addAfter <- el "div" $ dynButton "+"
  (comb,tPatTuple) <- el "div" $ do
      (c,_) <- liftM splitDynPure $ patternCombinatorDropDown' iPatComb never
      t <- resettableTransformedPatternWidget iTransPat never  -- this one has to have the 'reset' wrapper around it
      return (c,t)
  let tPat = fmap (\(x,_,_)->x) tPatTuple
  addInbetweenVal <- do
    let x = (\sp c-> TransformedPattern (Combine sp Merge) . TransformedPattern (Combine sp c)) <$> sPat <*> comb
    return $ (\tp cons->cons tp) <$> tPat <*> x
  let tEv = switchPromptlyDyn $ fmap (\(_,ev,_)->ev) tPatTuple
  let tHint = switchPromptlyDyn $ fmap (\(_,_,h)->h) tPatTuple
  val <- do
    let x = (\x y -> TransformedPattern (Combine x y)) <$> sPat <*> comb
    return $ (\t cons -> cons t) <$> tPat <*> x
  let childDeleteMe = ffilter (\x ->case x of DeleteMe ->True; otherwise-> False) tEv
  let childIsEmpty = ffilter id $ attachPromptlyDynWith (\x _->case x of EmptyTransformedPattern->True;otherwise->False) tPat childDeleteMe
  let transVal = fmap (TransformedPattern NoTransformer) val
  let untransPat = fmap UntransformedPattern sPat
  let rebuildVal = leftmost [(tPat <$) delete, (untransPat <$) childIsEmpty, (transVal <$) transform, (addInbetweenVal <$) addAfter]
  val' <- liftM join $ holdDyn val rebuildVal
  return $ fmap (\x->(x, leftmost [(DeleteMe <$) delete, (RebuildMe <$) transform,(RebuildMe <$) childDeleteMe, (RebuildMe <$) addAfter],leftmost [tHint,sHint])) val'
transformedPat (TransformedPattern iPatTrans iTransPat) _ = do
  (patTrans,deleteTransformer) <- liftM splitDynPure $ patternTransformerWidget iPatTrans never
  tPatTuple <- el "div" $ resettableTransformedPatternWidget iTransPat never
  let transPat = fmap (\(x,_,_)->x) tPatTuple
  let tEv = switchPromptlyDyn $ fmap (\(_,ev,_)->ev) tPatTuple
  let deleteEvent = ffilter (\x->case x of DeleteMe->True;otherwise->False) tEv
  let tHint = switchPromptlyDyn $ fmap (\(_,_,x)->x) tPatTuple
  let rebuildVal = tagPromptlyDyn transPat $ leftmost [switchPromptlyDyn deleteTransformer, deleteEvent]
  let newTransPat = (\x y-> TransformedPattern x y) <$> patTrans <*> transPat
  val <- liftM join $ holdDyn newTransPat $ fmap constDyn rebuildVal
  return $ fmap (\x-> (x,leftmost [deleteEvent, (RebuildMe <$) rebuildVal],tHint)) val


paramWidget :: MonadWidget t m => PatternTransformer -> m (Dynamic t PatternTransformer)
paramWidget (Jux trans) = do
  trans' <- parameteredPatternTransformer trans never
  let val' = fmap (\(next,_)-> Jux next) trans'
  return val'
paramWidget (Every num trans) = do
  let attrs = fromList $ zip ["type","style"] ["number","width:25px"]
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn attrs) & textInputConfig_initialValue .~ (T.pack $ show num)
  let input' = _textInput_value input -- Dyn string
  let val = ffor input' (\x->maybe 1 id (readMaybe (T.unpack x)::Maybe Int))
  nextTrans <- parameteredPatternTransformer trans never
  let val' = (\k (next,_)-> Every k next) <$> val <*> nextTrans
  return val'
paramWidget (Slow i) = do
  let numer = numerator i
  let denom = denominator i
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:25px"])) & textInputConfig_initialValue .~ (T.pack $ show numer)
  let input' = _textInput_value input -- Dyn string
  input2 <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ (T.pack $ show denom)
  let input2' = _textInput_value input2 -- Dyn string
  let val = ffor input' (\x->maybe 1 id (readMaybe (T.unpack x)::Maybe Integer))
  let val2 = ffor input2' (\x-> maybe 1 id (readMaybe (T.unpack x)::Maybe Integer))
  return $ (\x y->  Slow ((x%y)::Rational)) <$> val <*> val2
paramWidget (Density i)= do
  let numer = numerator i
  let denom = denominator i
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"]))  & textInputConfig_initialValue .~ (T.pack $ show numer)
  let input' = _textInput_value input -- Dyn string
  input2 <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ (T.pack $ show denom)
  let input2' = _textInput_value input2 -- Dyn string
  let val = ffor input' (\x->maybe 1 id (readMaybe (T.unpack x)::Maybe Integer))
  let val2 = ffor input2' (\x-> maybe 1 id (readMaybe (T.unpack x)::Maybe Integer))
  return $ (\x y-> Density $ (x%y::Rational) ) <$> val <*> val2
paramWidget (DegradeBy i) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number")) & textInputConfig_initialValue .~ (T.pack $ show i)
  let input' = _textInput_value input -- Dyn string
  let val = ffor input' (\x->maybe 1 id (readMaybe (T.unpack x)::Maybe Double))
  let val' = ffor val (\k-> DegradeBy k)
  return val'
paramWidget (Chop i) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number")) & textInputConfig_initialValue .~ (T.pack $ show i)
  let input' = _textInput_value input -- Dyn string
  let val = ffor input' (\x->maybe 1 id (readMaybe (T.unpack x)::Maybe Int))
  let val' = ffor val (\k-> Chop k)
  return val'
paramWidget (Combine iSPat iPatComb) = do
  sPat <- popupSpecificPatternWidget iSPat never >>= (return . fmap (\(x,_,_)->x))
  comb <- patternCombinatorDropDown' iPatComb never >>= (return . fmap fst)
  return $ Combine <$> sPat <*> comb
paramWidget transformer = return $ constDyn transformer

parameteredPatternTransformer::MonadWidget t m => PatternTransformer -> Event t () -> m (Dynamic t (PatternTransformer, Event t (EditSignal a)))
parameteredPatternTransformer i _ = el "div" $ do
  let transMap = fromList $ zip [0::Int,1..] [NoTransformer,Rev,Slow 1, Density 1, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1,Combine (Speed $ Atom 1 Inert Once) Multiply]
  let ddMap = constDyn $ fromList $ zip [0::Int,1..] ["NoTransformer","Rev","Slow","Density","Brak","Every","Jux","Chop","Combine"]
  delete <- liftM (DeleteMe <$) $ button "-"
  dd <- dropdown (hack i) ddMap def
  let ddVal = _dropdown_value dd -- Dynamic int
  let ddWidget = fmap (\k ->case Data.Map.lookup k transMap of Just a->paramWidget a; otherwise -> paramWidget NoTransformer) ddVal  --Dynamic (m(dynamic transformer))
  let ddWidgetValEv = updated ddWidget -- Event (M (dyn Pattern))
  paramValue <- widgetHold (paramWidget i) ddWidgetValEv  -- m Dynamic t(m (Dynamic PatternTrans...))
  let paramValue' = join paramValue --Dyn PatternTransformer
  return $ fmap (\k->(k,delete)) paramValue'
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
    hack (Combine _ _) = 10
    hack _ = 0


paramWidget'::MonadWidget t m=>PatternTransformer -> m (Dynamic t PatternTransformer)
paramWidget' (Jux trans) = do
  trans' <- patternTransformerWidget trans never
  let val' = fmap (\(next,_)-> Jux next) trans'
  return val'
paramWidget' (Every num trans) = do
  let attrs = fromList $ zip ["type","style"] ["number","width:25px"]
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn attrs) & textInputConfig_initialValue .~ (T.pack $ show num)
  let input' = _textInput_value input -- Dyn string
  let val = ffor input' (\x->maybe 1 id (readMaybe (T.unpack x)::Maybe Int))
  nextTrans <- patternTransformerWidget trans never
  let val' = (\k (next,_)-> Every k next) <$> val <*> nextTrans
  return val'
paramWidget' x = paramWidget x


patternTransformerWidget :: (MonadWidget t m)=> PatternTransformer -> Event t () -> m (Dynamic t (PatternTransformer, Event t (EditSignal a)))
patternTransformerWidget iValue _ = elClass "div" "patternTransformerWidget-or-popupSpecificPatternWidget" $ mdo
  let popup = patternTransformerPopup [DeleteMe]
  openEv <- clickableSpanClass showTransformer' "primary-color code-font background" ()
  dynPopup <- liftM switchPromptlyDyn $ flippableWidget (return never) popup False $ updated dynOpen
  dynOpen <- toggle False $ leftmost [openEv,(() <$ )dynPopup]
  let changeEvents = fmap ((\x->case x of ChangeValue a->a;otherwise->NoTransformer) . fromJust) $ ffilter (\x->case x of Just (ChangeValue a) ->True;otherwise->False) dynPopup
  let deleteEvent = fmap (\x->case x of Just DeleteMe -> DeleteMe; otherwise->Close) $ ffilter (\x-> case x of Just (DeleteMe)->True;otherwise->False) dynPopup
  transformer <- holdDyn NoTransformer changeEvents
  let showTransformer = fmap hack transformer
  showTransformer' <- holdDyn (hack iValue)  $ updated showTransformer
  let widget = fmap paramWidget' transformer
  paramValue <- liftM join $ widgetHold (paramWidget' iValue) $ updated widget
  return $ fmap (\x->(x,deleteEvent)) paramValue
  where
    hack NoTransformer = " NoTransformer "
    hack Rev = " Rev "
    hack (Slow _) = " Slow " -- sorry...
    hack (Density _) = " Density "
    hack Degrade = " Degrade "
    hack (DegradeBy _) = " DegradeBy "
    hack Brak = " Brak "
    hack (Every _ _)= " Every "
    hack (Jux _) = " Jux "
    hack (Chop _) = " Chop "
    hack (Combine _ _) = " Combine "


patternTransformerPopup:: MonadWidget t m => [EditSignal PatternTransformer] -> m (Event t (Maybe (EditSignal PatternTransformer)))
patternTransformerPopup actionList = elClass "div" "popupMenu" $ do
  let transMap = fromList $ zip [0::Int,1..] [NoTransformer,Rev,Slow 1, Density 1, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1,Combine (Speed $ Atom 1 Inert Once) Multiply]
  let ddMap = constDyn $ fromList $ zip [0::Int,1..] ["NoTransformer","Rev","Slow","Density","Brak","Every","Jux","Chop","Combine"]

  dd <- dropdown (-1) ddMap def
  let transformerKey = _dropdown_value dd
  let transformerChange = fmap (Just . ChangeValue . maybe NoTransformer id . (flip Data.Map.lookup) transMap) transformerKey
  let popupList = fmap (\x->clickableDivClass' (T.pack $ show x) "primary-color code-font background" (Just x)) actionList -- [m (Maybe (EditSignal))]
  edits <- liftM id $ Control.Monad.sequence popupList
  closeMenu <- clickableDivClass' "close" "primary-color code-font background" (Nothing)
  return $ (leftmost $ edits ++[closeMenu,updated transformerChange])
