{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.TransformedPattern where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.PatternTransformer
import Estuary.Widgets.Generic
import Estuary.Reflex.Container
import Control.Monad
import Data.Map
import Data.List
import qualified Estuary.Widgets.SpecificPattern as Sp
import GHC.Real

import Text.Read
import Estuary.Reflex.Utility


dropdownPatternWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t ()))
dropdownPatternWidget iPattern _ = do
  let paramShowList = ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"] -- Map (Map k func) String
  let patternType = head $ words $ show iPattern
  let initialIndex = maybe (0) id $ Data.List.findIndex (==patternType) paramShowList -- Int of initalFunc
  let patMap = fromList $ zip [0..] builderList
  let initialFunc = maybe (builderList!!0) (id) $ Data.Map.lookup initialIndex patMap
  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] paramShowList
  patternDropDown <- dropdown initialIndex dropDownMap def
  let ddVal = _dropdown_value patternDropDown
  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> Sp.specificContainer (S $ Blank Inert) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn )
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


--transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t (EditSignal a)))
--transformedPatternWidget (TransformedPattern ts p) _ = el "div" $ do
--  deleteEvents <- buttonDynAttrs "-" (DeleteMe) (constDyn $ "class"=:"patternAddButton")
--  (soundPattern,events) <- dropdownPatternWidget (p) never >>= splitDyn
--  --deleteEvents <- mapDyn (ffilter (==DeleteMe)) events --Dyn Event DeleteMe
--  transformer <- parameteredPatternTransformer (f ts) never
--  transformedPat <- combineDyn(\(a,_) b-> TransformedPattern [a] b) transformer soundPattern -- Dyn transformedPat
--  mapDyn (\a-> (a,deleteEvents)) transformedPat
--  where
--    f [] = NoTransformer -- sorry again...
--    f (x:_) = x







-- @parameteredPatternTransformer only takes 1 pattern transformer rn, should probably take a potentially infinite number of them
-- (so we can have more than one pattern transformation per pattern)
-- how to handle deletion though? - TransformedPattern Chain hsa to be reconfigured

-- ++++
--transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t (EditSignal a)))
--transformedPatternWidget iTransPat _ = el "div" $ do
--  deleteButton <- button "-"
--  deleted <- toggle False deleteButton
--  patTrans <- parameteredPatternTransformer transformer never
--  --let regWidget = do
--  --    tPat <- dropdownPatternWidget iSpecPat never
--  --    mapDyn (\(x,y)->(x,y,Merge)) tPat

--  (specPat, events) <- dropdownPatternWidget iSpecPat never >>= splitDyn


--  --resettableWidget :: MonadWidget t m => (a -> Event t b -> m (Dynamic t c)) -> a -> Event t b -> Event t a -> m (Dynamic t c)


--  --combine <- button "+" >>= (transformedPatternWidget (UntransformedPattern specPat) never <$) 
--  combine <- button "+" 
--  isCombined <- toggle False combine --  >>= combineDyn (\del com -> if del then False else com)

--  --flippableWidget :: MonadWidget t m => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)

--  let nextBuilder = do
--      c <- patternCombinatorDropDown Merge never >>= mapDyn fst 
--      --mapDyn show c >>= dynText
--      (v,e) <- transformedPatternWidget (UntransformedPattern iSpecPat) never >>= splitDyn
--      combineDyn (,) v e >>= combineDyn (\comb (x,y)-> (x,y,comb)) c

--  nextWidget <- liftM joinDyn $ flippableWidget (return $ constDyn (EmptyTransformedPattern,never,Merge)) nextBuilder False (updated isCombined)
--  --val <- flippableWidget nextWidget regWidget False (updated deleted))
--  combineVal <- combineDyn (\a (b,c,d)->(a,b,c,d)) isCombined nextWidget

--  nextWidgetVal <- mapDyn (\(x,y,_)->(x,y)) nextWidget

--  let resetEvent = tagDyn nextWidgetVal deleteButton

--  val <- resettableWidget dropdownPatternWidget iSpecPat never resetEvent

--  val' <- combineDyn (\(a,_) b->(a,b)) patTrans specPat
--  pat <- combineDyn (\(tog,cVal,ev,comb) (pT,spV) -> if tog then (TransformedPattern pT (TransformedPattern (Combine spV comb) cVal),ev) else (TransformedPattern pT $ UntransformedPattern spV,never)) combineVal val'
--  mapDyn (show . fst) pat >>= dynText
--  return pat
--  where
--    (transformer,iSpecPat) = case iTransPat of
--      (TransformedPattern t (UntransformedPattern s)) -> (t,s)
--      (UntransformedPattern s) -> (NoTransformer,s)
--      (EmptyTransformedPattern) -> (NoTransformer,S $ Blank Inert Once)
-- ++++





---- @parameteredPatternTransformer only takes 1 pattern transformer rn, should probably take a potentially infinite number of them
---- (so we can have more than one pattern transformation per pattern)
---- how to handle deletion though? - TransformedPattern Chain hsa to be reconfigured
--transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t (EditSignal a)))
--transformedPatternWidget iTransPat _ = el "div" $ do
--  deleteButton <- button "-"

--  deleted <- toggle False deleteButton
--  patTrans <- parameteredPatternTransformer transformer never

--  let regWidget = do
--      tPat <- dropdownPatternWidget iSpecPat never
--      mapDyn (\(x,y)->(x,y,Merge)) tPat

--  --(specPat, events) <- dropdownPatternWidget iSpecPat never >>= splitDyn

--  --combine <- button "+" >>= (transformedPatternWidget (UntransformedPattern specPat) never <$) 
--  combine <- button "+" 
--  isCombined <- toggle False combine --  >>= combineDyn (\del com -> if del then False else com)

--  --flippableWidget :: MonadWidget t m => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)
--  let nextBuilder = do
--      c <- patternCombinatorDropDown Merge never >>= mapDyn fst 
--      --mapDyn show c >>= dynText
--      (v,e) <- transformedPatternWidget (UntransformedPattern iSpecPat) never >>= splitDyn
--      combineDyn (,) v e >>= combineDyn (\comb (x,y)-> (x,y,comb)) c

--  nextWidget <- liftM joinDyn $ flippableWidget (return $ constDyn (EmptyTransformedPattern,never,Merge)) nextBuilder False (updated isCombined)
  
--  (specPat,_) <- liftM joinDyn (flippableWidget nextWidget regWidget False (updated deleted)) >>=splitDyn


--  combineVal <- combineDyn (\a (b,c,d)->(a,b,c,d)) isCombined nextWidget

--  val' <- combineDyn (\(a,_) b->(a,b)) patTrans specPat
--  pat <- combineDyn (\(tog,cVal,ev,comb) (pT,spV) -> if tog then (TransformedPattern (Combine spV comb) cVal,ev) else (TransformedPattern pT $ UntransformedPattern spV,never)) combineVal val'
--  mapDyn (show . fst) pat >>= dynText
--  return pat
--  where
--    (transformer,iSpecPat) = case iTransPat of
--      (TransformedPattern t (UntransformedPattern s)) -> (t,s)
--      (UntransformedPattern s) -> (NoTransformer,s)
--      (EmptyTransformedPattern) -> (NoTransformer,S $ Blank Inert Once)



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

--transformedPatternWidget' transformedPat ev = el "div" $ do
--  (specPat, events) <- dropdownPatternWidget iSpecPat never >>= splitDyn
--  patTrans <- parameteredPatternTransformer' transformer never

--  where
--    transformer = case transformedPat of
--      (TransformedPattern t _) -> t  -- @ should really return a list of all the transformations applied to the pattern
--      (UntransformedPattern _) -> NoTransformer

 
--transformedPatternWidget' :: MonadWidget t m => TransformedPattern -> m (Dynamic t TransformedPattern)

--transformedPatternWidget' (UntransformedPattern u) = do
--  b <- button "transformMe"
--  transformedPatternWidget'' (UntransformedPattern u)

--transformedPatternWidget'' (UntransformedPattern u) = do
--  u' <- specificPatternWidget u
--  mapDyn UntransformedPattern u'

--transformedPatternWidget' (TransformedPattern t x) = do
--  b <- button "transformMe" >>= (   <$)
--  transformedPatternWidget'' (TransformedPattern t x)

--transformedPatternWidget'' (TransformedPattern t x) = do
--  t' <- patternTransformedWidget t
--  x' <- transformedPatternWidget' x
--  combineDyn TransformedPattern t' x'













transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t (EditSignal a) -> m (Dynamic t(TransformedPattern, Event t (EditSignal a)))
transformedPatternWidget iTransPat ev = mdo
  val <- resettableWidget (transformedPat) iTransPat ev resetEvent
  tPat <- mapDyn fst val
  e <- liftM switchPromptlyDyn $ mapDyn snd val
  let resetEvent = tagDyn tPat $ ffilter (\x-> case x of RebuildMe->True; otherwise -> False) e
  mapDyn (\x->(x,e)) tPat

--resettableWidget :: MonadWidget t m => (a -> Event t b -> m (Dynamic t c)) -> a -> Event t b -> Event t a -> m (Dynamic t c)


transformedPat :: MonadWidget t m => TransformedPattern -> Event t (EditSignal a) -> m (Dynamic t (TransformedPattern, Event t (EditSignal a)))
transformedPat (EmptyTransformedPattern) _ = do
  x <- liftM (UntransformedPattern (S (Atom "~" Inert Once))  <$) $ button "make me not empty"
  value <- holdDyn EmptyTransformedPattern x
  let event = (RebuildMe <$) x
  mapDyn (\y -> (y,event)) value

transformedPat (UntransformedPattern specificPattern) _= do
  
  delete <- button "delete"
  transform <- button "transform" -- >>= toggle False 
  sPat <- dropdownPatternWidget specificPattern never >>= mapDyn fst
  tPat <- mapDyn (UntransformedPattern) sPat
  combine <- button "+"
  let delete' = (EmptyTransformedPattern <$) delete
  let updatedValue = attachDynWith (\sp _-> constDyn $ TransformedPattern NoTransformer sp) tPat transform
  let combineValue = attachDynWith (\sp trans-> constDyn $ TransformedPattern (Combine sp Merge) $ UntransformedPattern (S $ Atom "bd" Inert (Rep 4))) sPat combine

  --transformer <- liftM (fmap fst . updated . joinDyn) $ flippableWidget (parameteredPatternTransformer NoTransformer never) (return $ constDyn (NoTransformer,())) False $ updated transform
  -- ^ an event, firing whenever the 'transformer' changes, containing the transformer

  --let updatedValue = attachDynWith (\sp trans-> constDyn $ TransformedPattern trans sp) tPat transformer

  let updatedValue' = leftmost [updatedValue, combineValue, fmap constDyn delete']
  --unTransPat <- mapDyn UntransformedPattern specPat
  value <- liftM joinDyn $ holdDyn tPat updatedValue'
  --let transform' = (TransformedPattern Brak ...and the current value from x... <$) transform
  --let rebuildEvents = leftmost [delete'', fmap (const RebuildMe) transformer]
  let rebuildEvents = leftmost [(RebuildMe <$) delete, (RebuildMe <$) transform, (RebuildMe <$) combine]

  mapDyn (\x->(x,rebuildEvents)) value


--transformedPatternWidget (TransformedPattern transformer pattern) = do
--  x <- rebuildableWidget transformerWidget
--  y <- rebuildableWidget transformedPatternWidget -- with pattern at first, then with whatever happens
   --just like the untransformed pattern 

transformedPat (TransformedPattern (Combine iSpecPat iPatComb) iTransPat) _ = do  
  delete <- button "delete"
  (specPat,sEv) <- dropdownPatternWidget iSpecPat never >>= splitDyn
  (comb,_) <- patternCombinatorDropDown iPatComb never >>= splitDyn
  (transPat,transEv) <- transformedPatternWidget iTransPat never >>= splitDyn  -- this one has to have the 'reset' wrapper around it
  --let ev = leftmost $ fmap switchPromptlyDyn $ [sEv, transEv]
  val <- combineDyn (\x y -> TransformedPattern (Combine x y)) specPat comb >>= combineDyn (\t cons -> cons t) transPat
  val' <- liftM joinDyn $ holdDyn val $ fmap (const transPat) delete
  mapDyn (\x->(x,(RebuildMe <$) delete)) val'

transformedPat (TransformedPattern iPatTrans iTransPat) _ = do
  iPatTrans <- parameteredPatternTransformer iPatTrans never >>= mapDyn fst
  transPat <- transformedPatternWidget iTransPat never >>= mapDyn fst
  combineDyn (\x y-> (TransformedPattern x y,never)) iPatTrans transPat

--transformedPat (TransformedPattern iTransformer iTransPat) _ = do

--TransformedPattern (Combine s "bd" Merge) $ TransformedPattern

--transformedPat (TransformedPattern iPatTrans iTransPat) _ = do

--  patTrans <- patternCombinatorDropDown iPatTrans never
--  transPat <- transformedPat iTransPat never

--  every 2 (brak) $ s "bd cp" # speed "2"

--  TransformedPattern (every 2 brak) $ UntransformedPattern (s "bd cp")

--  TransformedPattern (every 2 (brak)) $ TransformedPattern (Combine $ s "bd cp") $ UntransformedPattern (speed "2")




--data TransformedPattern = TransformedPattern PatternTransformer TransformedPattern | UntransformedPattern SpecificPattern | EmptyTransformedPattern deriving (Eq)
--  Combine SpecificPattern PatternCombinator



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
paramWidget (Combine iSPat iPatComb) = do
  sPat <- dropdownPatternWidget iSPat never >>= mapDyn fst
  comb <- patternCombinatorDropDown iPatComb never >>= mapDyn fst
  combineDyn Combine sPat comb
paramWidget transformer = return $ constDyn transformer

parameteredPatternTransformer::MonadWidget t m => PatternTransformer -> Event t () -> m (Dynamic t (PatternTransformer, ()))
parameteredPatternTransformer i _ = el "div" $ do
  let transMap = fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9,10] [NoTransformer,Rev,Slow 1, Density 1, Degrade, DegradeBy 0.5, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1,Combine (Speed $ Atom 1 Inert Once) Multiply]
  let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9,10] ["NoTransformer","Rev","Slow","Density", "Degrade", "DegradeBy","Brak","Every","Jux","Chop","Combine"]
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
    hack (Combine _ _) = 10
    hack _ = 0



