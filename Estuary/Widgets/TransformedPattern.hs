{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.TransformedPattern where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.WebDirt.Foreign
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Reflex.Container
import Control.Monad
import Data.Map
import Data.List
import qualified Estuary.Widgets.SpecificPattern as Sp
import GHC.Real
import Data.Maybe (fromJust)
import Text.Read

import Estuary.Utility (lastOrNothing)
import Estuary.Types.Hint

topLevelTransformedPatternWidget :: MonadWidget t m =>
  TransformedPattern -> -- initial value
  Event t [TransformedPattern] -> -- deltas from network (must not re-propagate as edit events!)
  m (
    Dynamic t TransformedPattern, -- value for local WebDirt playback
    Event t TransformedPattern, -- deltas to network (not based on events received from network!)
    Event t Hint -- hints (currently for WebDirt sample loading only)
  )
topLevelTransformedPatternWidget i delta = do
  let updates = fmap (midLevelTransformedPatternWidget) $ fmapMaybe lastOrNothing delta
  w <- widgetHold (midLevelTransformedPatternWidget i) updates
  x <- mapDyn (\(a,_,_) -> a) w
  y <- mapDyn (\(_,a,_) -> a) w
  z <- mapDyn (\(_,_,a) -> a) w
  let x' = joinDyn x
  let y' = switchPromptlyDyn y
  let z' = switchPromptlyDyn z
  return (x',y',z')


midLevelTransformedPatternWidget:: MonadWidget t m =>
  TransformedPattern -> m (Dynamic t TransformedPattern, Event t TransformedPattern, Event t Hint)
midLevelTransformedPatternWidget iTransPat = do
  tuple <- resettableTransformedPatternWidget iTransPat never
  pat <- mapDyn (\(x,_,_)->x) tuple
  --ev <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_)->x) tuple
  let ev = updated pat
  --holdDyn "no update yet" (fmap (const "update received") ev) >>= dynText
  hint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x)->x) tuple
  return (pat,ev,hint)


popupSpecificPatternWidget :: (MonadWidget t m)=> SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (EditSignal a),Event t Hint))
popupSpecificPatternWidget iValue _ = elClass "div" "popupSpecificPatternWidget" $ mdo
  let popup = specificPatternPopup [DeleteMe, TransformMe]
  openEv <- clickableSpanClass showSpecPat "noClass" ()
  dynPopup <- liftM switchPromptlyDyn $ flippableWidget (return never) popup False $ updated dynOpen
  dynOpen <- toggle False $ leftmost [openEv,(() <$ )dynPopup]
  let changeEvents = fmap ((\x->case x of ChangeValue a->a;otherwise->(S $ Blank Inert)) . fromJust) $ ffilter (\x->case x of Just (ChangeValue a) ->True;otherwise->False) dynPopup
  let deleteEvent = fmap (\x->case x of Just DeleteMe -> DeleteMe; otherwise->Close) $ ffilter (\x-> case x of Just (DeleteMe)->True;otherwise->False) dynPopup
  let transformEvent = fmap (\x->case x of Just TransformMe -> TransformMe; otherwise->Close) $ ffilter (\x-> case x of Just (TransformMe)->True; otherwise->False) dynPopup
  --specPat <- holdDyn iValue changeEvents >>= liftM joinDyn $ mapDyn (flip  Sp.specificPattern $ never)
  specPatTuple <- liftM joinDyn $ widgetHold (Sp.specificContainer iValue never) $ fmap (flip Sp.specificContainer $ never) changeEvents
  specPat <- mapDyn (\(x,_,_)->x) specPatTuple
  showSpecPat <- mapDyn hack specPat
  mapDyn (\(x,y,z)-> (x,leftmost [transformEvent,deleteEvent],z)) specPatTuple
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
  --let ddMap = constDyn $ fromList $ zip [(0::Int)..] ["bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"] -- Map (Map k func) String
  dd <- dropdown (-1) ddMap def
  let specPatKey = _dropdown_value dd
  specChange <- mapDyn (Just . ChangeValue . maybe (S $ Blank Inert) id . (flip Data.Map.lookup) specMap) specPatKey
  let popupList = fmap (\x->clickableDivClass' (show x) "noClass" (Just x)) actionList -- [m (Maybe (EditSignal))]
  edits <- liftM id $ Control.Monad.sequence popupList
  closeMenu <- clickableDivClass' "close" "noClass" (Nothing)
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


dropdownPatternWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (),Event t Hint))
dropdownPatternWidget iPattern _ = do
  let paramShowList = ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"]
  let patternType = head $ words $ show iPattern
  let initialIndex = maybe (0) id $ Data.List.findIndex (==patternType) paramShowList -- Int of initalFunc
  let patMap = Data.Map.insert initialIndex (Sp.specificContainer iPattern never) $ fromList $ zip [0..] builderList
  let initialFunc = maybe (builderList!!0) (id) $ Data.Map.lookup initialIndex patMap
  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] paramShowList
  patternDropDown <- dropdown initialIndex dropDownMap def
  let ddVal = _dropdown_value patternDropDown
  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> Sp.specificContainer (S $ Blank Inert) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn ,ev,ev)
  soundPatEv' <- widgetHold (initialFunc) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)
  return $ soundPattern
  where
    builderList = Prelude.map (\x-> Sp.specificContainer x never) [(Accelerate $ Atom 0 Inert Once),
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
      (Vowel $ Atom 'o' Inert Once)] --, stringContainerWidget (Unit $ Atom "c" Once),stringContainerWidget (Vowel $ Atom "c" Once)]


patternCombinatorDropDown :: MonadWidget t m => PatternCombinator -> Event t () -> m (Dynamic t (PatternCombinator,Event t (EditSignal a)))
patternCombinatorDropDown iValue _ = do
  let ddMapVals = fromList $ zip [(1::Int)..] [Merge,Add,Subtract,Multiply,Divide]
  let ddMap = constDyn $ fromList $ zip [(1::Int)..] $ fmap show [Merge,Add,Subtract,Multiply,Divide]
  dd <- dropdown iIndex ddMap def
  --mapDyn show (_dropdown_value dd) >>= dynText
  let choice = _dropdown_value dd
  val <- mapDyn (maybe Merge id . (flip Data.Map.lookup) ddMapVals) choice
  mapDyn (\x->(x,never)) val
  --mapDyn ((flip Data.Map.lookup) ddMapVals) choice >>= mapDyn (\x -> (x,never))
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
  tPat <- mapDyn (\(x,_,_)->x) val
  e <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_)->x) val
  h <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x)->x) val
  let resetEvent = tagDyn tPat $ ffilter (\x-> case x of RebuildMe->True; DeleteMe -> True; otherwise -> False) e
  mapDyn (\x->(x,e,h)) tPat


transformedPat :: MonadWidget t m => TransformedPattern -> Event t (EditSignal a) -> m (Dynamic t (TransformedPattern, Event t (EditSignal a), Event t Hint))
transformedPat (EmptyTransformedPattern) _ = do
  x <- liftM (UntransformedPattern (S (Atom "~" Inert Once))  <$) $ button "+"
  value <- holdDyn EmptyTransformedPattern x
  let event = (RebuildMe <$) x
  mapDyn (\y -> (y,event,never)) value
transformedPat (UntransformedPattern specificPattern) _= do
  --delete <- button "-"
  --transform <- button "transform"
  sPatTuple <- popupSpecificPatternWidget specificPattern never
  sPat <- mapDyn (\(x,_,_)->x) sPatTuple
  sEv <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_)->x) sPatTuple
  let delete = ffilter (\x->case x of DeleteMe->True;otherwise->False) sEv
  let transform = ffilter (\x->case x of TransformMe->True;otherwise->False) sEv

  hint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h)->h) sPatTuple
  tPat <- mapDyn (UntransformedPattern) sPat
  combine <- el "div" $ button "+"
  let delete' = (EmptyTransformedPattern <$) delete
  let updatedValue = attachDynWith (\sp _-> constDyn $ TransformedPattern NoTransformer sp) tPat transform
  let combineValue = attachDynWith (\sp trans-> constDyn $ TransformedPattern (Combine sp Merge) $ UntransformedPattern (Speed $ Atom  1 Inert Once)) sPat combine
  --let addBefore' = attachDynWith (\sp _->constDyn $ TransformedPattern (Combine sp Merge) $ UntransformedPattern sp) sPat addBefore
  let updatedValue' = leftmost [updatedValue, combineValue, fmap constDyn delete']
  value <- liftM joinDyn $ holdDyn tPat updatedValue'
  let rebuildEvents = leftmost [(DeleteMe <$) delete, (RebuildMe <$) transform, (RebuildMe <$) combine]
  mapDyn (\x->(x,rebuildEvents,hint)) value
transformedPat (TransformedPattern (Combine iSpecPat iPatComb) EmptyTransformedPattern) _ = transformedPat (UntransformedPattern iSpecPat) never
transformedPat (TransformedPattern (Combine iSpecPat iPatComb) iTransPat) _ = do
  --delete <- button "-"
  --transform <- button "transform"
  sPatTuple <- popupSpecificPatternWidget iSpecPat never
  sPat <- mapDyn (\(x,_,_)->x) sPatTuple
  sHint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h)->h) sPatTuple
  sEv <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_)->x) sPatTuple
  let delete = ffilter (\x->case x of DeleteMe->True;otherwise->False) sEv
  let transform = ffilter (\x->case x of TransformMe->True;otherwise->False) sEv
  addAfter <- el "div" $ button "+"
  (comb,tPatTuple) <- el "div" $ do
      (c,_) <- patternCombinatorDropDown' iPatComb never >>= splitDyn
      t <- resettableTransformedPatternWidget iTransPat never  -- this one has to have the 'reset' wrapper around it
      return (c,t)

  tPat <- mapDyn (\(x,_,_)->x) tPatTuple
  addInbetweenVal <- combineDyn (\sp c-> TransformedPattern (Combine sp Merge) . TransformedPattern (Combine sp c)) sPat comb >>= combineDyn (\tp cons->cons tp) tPat
  tEv <- liftM switchPromptlyDyn $ mapDyn (\(_,ev,_)->ev) tPatTuple
  tHint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h)->h) tPatTuple
  val <- combineDyn (\x y -> TransformedPattern (Combine x y)) sPat comb >>= combineDyn (\t cons -> cons t) tPat
  let childDeleteMe = ffilter (\x ->case x of DeleteMe ->True; otherwise-> False) tEv
  let childIsEmpty = ffilter id $ attachDynWith (\x _->case x of EmptyTransformedPattern->True;otherwise->False) tPat childDeleteMe
  transVal <- mapDyn (TransformedPattern NoTransformer) val
  untransPat <- mapDyn UntransformedPattern sPat
  let rebuildVal = leftmost [(tPat <$) delete, (untransPat <$) childIsEmpty, (transVal <$) transform, (addInbetweenVal <$) addAfter]
  val' <- liftM joinDyn $ holdDyn val rebuildVal
  mapDyn (\x->(x, leftmost [(DeleteMe <$) delete, (RebuildMe <$) transform,(RebuildMe <$) childDeleteMe, (RebuildMe <$) addAfter],leftmost [tHint,sHint])) val'
transformedPat (TransformedPattern iPatTrans iTransPat) _ = do
  (patTrans,deleteTransformer) <- patternTransformerWidget iPatTrans never >>= splitDyn
  tPatTuple <- el "div" $ resettableTransformedPatternWidget iTransPat never
  transPat <- mapDyn (\(x,_,_)->x) tPatTuple
  tEv <- liftM switchPromptlyDyn $ mapDyn (\(_,ev,_)->ev) tPatTuple
  let deleteEvent = ffilter (\x->case x of DeleteMe->True;otherwise->False) tEv
  tHint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x)->x) tPatTuple
  let rebuildVal = tagDyn transPat $ leftmost [switchPromptlyDyn deleteTransformer, deleteEvent]
  newTransPat <- combineDyn (\x y-> TransformedPattern x y) patTrans transPat
  val <- liftM joinDyn $ holdDyn newTransPat $ fmap constDyn rebuildVal
  mapDyn (\x-> (x,leftmost [deleteEvent, (RebuildMe <$) rebuildVal],tHint)) val





--transformedPat (TransformedPattern (Combine iSpecPat iPatComb) iTransPat) _ = do
--  delete <- button "-"
--  transform <- button "transform"
--  sPatTuple <- dropdownPatternWidget iSpecPat never
--  sPat <- mapDyn (\(x,_,_)->x) sPatTuple
--  sHint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h)->h) sPatTuple
--  --addAfter <- el "div" $ button "add"
--  (comb,tPatTuple) <- el "div" $ do
--      (c,_) <- patternCombinatorDropDown iPatComb never >>= splitDyn
--      t <- resettableTransformedPatternWidget iTransPat never  -- this one has to have the 'reset' wrapper around it
--      return (c,t)
--  tPat <- mapDyn (\(x,_,_)->x) tPatTuple
--  tEv <- liftM switchPromptlyDyn $ mapDyn (\(_,ev,_)->ev) tPatTuple
--  tHint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h)->h) tPatTuple
--  val <- combineDyn (\x y -> TransformedPattern (Combine x y)) sPat comb >>= combineDyn (\t cons -> cons t) tPat
--  let childDeleteMe = ffilter (\x ->case x of DeleteMe ->True; otherwise-> False) tEv
--  let childIsEmpty = ffilter id $ attachDynWith (\x _->case x of EmptyTransformedPattern->True;otherwise->False) tPat childDeleteMe
--  transVal <- mapDyn (TransformedPattern NoTransformer) val
--  untransPat <- mapDyn UntransformedPattern sPat
--  val' <- liftM joinDyn $ holdDyn val $ fmap (const tPat) delete
--  val'' <- liftM joinDyn $ holdDyn val' $ (transVal <$) transform
--  val'''<- liftM joinDyn $ holdDyn val'' $ (untransPat <$) childIsEmpty
--  mapDyn (\x->(x, leftmost [(DeleteMe <$) delete, (RebuildMe <$) transform,(RebuildMe <$) childDeleteMe],leftmost [tHint,sHint])) val'''





paramWidget::MonadWidget t m=>PatternTransformer -> m (Dynamic t PatternTransformer)
paramWidget (Jux trans) = do
  trans' <- parameteredPatternTransformer trans never
  val'<- mapDyn (\(next,_)-> Jux next) trans'
  return val'
paramWidget (Every num trans) = do
  let attrs = fromList $ zip ["type","style"] ["number","width:25px"]
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn attrs) & textInputConfig_initialValue .~ (show num)
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Int))
  nextTrans <- parameteredPatternTransformer trans never
  val'<-combineDyn (\k (next,_)-> Every k next) val nextTrans
  return val'
paramWidget (Slow i) = do
  let numer = numerator i
  let denom = denominator i
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:25px"])) & textInputConfig_initialValue .~ (show numer)
  let input' = _textInput_value input -- Dyn string
  input2 <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ (show denom)
  let input2' = _textInput_value input2 -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Integer))
  val2 <- forDyn input2' (\x-> maybe 1 id (readMaybe x::Maybe Integer))
  combineDyn (\x y->  Slow ((x%y)::Rational)) val val2
paramWidget (Density i)= do
  let numer = numerator i
  let denom = denominator i
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"]))  & textInputConfig_initialValue .~ (show numer)
  let input' = _textInput_value input -- Dyn string
  input2 <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ (show denom)
  let input2' = _textInput_value input2 -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Integer))
  val2 <- forDyn input2' (\x-> maybe 1 id (readMaybe x::Maybe Integer))
  combineDyn (\x y-> Density $ (x%y::Rational) ) val val2
paramWidget (DegradeBy i) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number")) & textInputConfig_initialValue .~ (show i)
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Double))
  val'<-forDyn val (\k-> DegradeBy k)
  return val'
paramWidget (Chop i) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number")) & textInputConfig_initialValue .~ (show i)
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Int))
  val'<-forDyn val (\k-> Chop k)
  return val'
paramWidget (Combine iSPat iPatComb) = do
  sPat <- popupSpecificPatternWidget iSPat never >>= mapDyn (\(x,_,_)->x)
  comb <- patternCombinatorDropDown' iPatComb never >>= mapDyn fst
  combineDyn Combine sPat comb
paramWidget transformer = return $ constDyn transformer

parameteredPatternTransformer::MonadWidget t m => PatternTransformer -> Event t () -> m (Dynamic t (PatternTransformer, Event t (EditSignal a)))
parameteredPatternTransformer i _ = el "div" $ do
  --let transMap = fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9,10] [NoTransformer,Rev,Slow 1, Density 1, Degrade, DegradeBy 0.5, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1,Combine (Speed $ Atom 1 Inert Once) Multiply]
  --let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9,10] ["NoTransformer","Rev","Slow","Density", "Degrade", "DegradeBy","Brak","Every","Jux","Chop","Combine"]
  let transMap = fromList $ zip [0::Int,1..] [NoTransformer,Rev,Slow 1, Density 1, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1,Combine (Speed $ Atom 1 Inert Once) Multiply]
  let ddMap = constDyn $ fromList $ zip [0::Int,1..] ["NoTransformer","Rev","Slow","Density","Brak","Every","Jux","Chop","Combine"]
  delete <- liftM (DeleteMe <$) $ button "-"
  dd <- dropdown (hack i) ddMap def
  let ddVal = _dropdown_value dd -- Dynamic int
  ddWidget <- mapDyn (\k ->case Data.Map.lookup k transMap of Just a->paramWidget a; otherwise -> paramWidget NoTransformer) ddVal  --Dynamic (m(dynamic transformer))
  let ddWidgetValEv =  updated ddWidget -- Event (M (dyn Pattern))
  paramValue <- widgetHold (paramWidget i) ddWidgetValEv  -- m Dynamic t(m (Dynamic PatternTrans...))
  let paramValue' = joinDyn paramValue --Dyn PatternTransformer
  mapDyn (\k->(k,delete)) paramValue'
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
  val'<- mapDyn (\(next,_)-> Jux next) trans'
  return val'
paramWidget' (Every num trans) = do
  let attrs = fromList $ zip ["type","style"] ["number","width:25px"]
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn attrs) & textInputConfig_initialValue .~ (show num)
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Int))
  nextTrans <- patternTransformerWidget trans never
  val'<-combineDyn (\k (next,_)-> Every k next) val nextTrans
  return val'
paramWidget' x = paramWidget x



patternTransformerWidget :: (MonadWidget t m)=> PatternTransformer -> Event t () -> m (Dynamic t (PatternTransformer, Event t (EditSignal a)))
patternTransformerWidget iValue _ = elClass "div" "patternTransformerWidget" $ mdo
  let popup = patternTransformerPopup [DeleteMe]
  openEv <- clickableSpanClass showTransformer' "noClass" ()
  dynPopup <- liftM switchPromptlyDyn $ flippableWidget (return never) popup False $ updated dynOpen
  dynOpen <- toggle False $ leftmost [openEv,(() <$ )dynPopup]
  let changeEvents = fmap ((\x->case x of ChangeValue a->a;otherwise->NoTransformer) . fromJust) $ ffilter (\x->case x of Just (ChangeValue a) ->True;otherwise->False) dynPopup
  let deleteEvent = fmap (\x->case x of Just DeleteMe -> DeleteMe; otherwise->Close) $ ffilter (\x-> case x of Just (DeleteMe)->True;otherwise->False) dynPopup
  transformer <- holdDyn NoTransformer changeEvents
  showTransformer <- mapDyn hack transformer
  showTransformer' <-holdDyn (hack iValue)  $ updated showTransformer
  widget <- mapDyn paramWidget' transformer
  paramValue <- liftM joinDyn $ widgetHold (paramWidget' iValue) $ updated widget

  mapDyn (\x->(x,deleteEvent)) paramValue
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
  --let transMap = fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9,10] [NoTransformer,Rev,Slow 1, Density 1, Degrade, DegradeBy 0.5, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1,Combine (Speed $ Atom 1 Inert Once) Multiply]
  --let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9,10] ["NoTransformer","Rev","Slow","Density", "Degrade", "DegradeBy","Brak","Every","Jux","Chop","Combine"]
  let transMap = fromList $ zip [0::Int,1..] [NoTransformer,Rev,Slow 1, Density 1, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1,Combine (Speed $ Atom 1 Inert Once) Multiply]
  let ddMap = constDyn $ fromList $ zip [0::Int,1..] ["NoTransformer","Rev","Slow","Density","Brak","Every","Jux","Chop","Combine"]

  --let combinatorMap = fromList $ zip [0::Int..] [NoTransformer,Rev,Slow 1, Density 1, Degrade, DegradeBy 0.5, Every ]
  dd <- dropdown (-1) ddMap def
  let transformerKey = _dropdown_value dd
  transformerChange <- mapDyn (Just . ChangeValue . maybe NoTransformer id . (flip Data.Map.lookup) transMap) transformerKey
  let popupList = fmap (\x->clickableDivClass' (show x) "noClass" (Just x)) actionList -- [m (Maybe (EditSignal))]
  edits <- liftM id $ Control.Monad.sequence popupList
  closeMenu <- clickableDivClass' "close" "noClass" (Nothing)
  return $ (leftmost $ edits ++[closeMenu,updated transformerChange])
