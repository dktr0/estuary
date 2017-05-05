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
transformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t (EditSignal a)))
transformedPatternWidget iTransPat _ = el "div" $ do
  deleteButton <- button "-"
  deleted <- toggle False deleteButton
  patTrans <- parameteredPatternTransformer transformer never
  --let regWidget = do
  --    tPat <- dropdownPatternWidget iSpecPat never
  --    mapDyn (\(x,y)->(x,y,Merge)) tPat

  (specPat, events) <- dropdownPatternWidget iSpecPat never >>= splitDyn


  --resettableWidget :: MonadWidget t m => (a -> Event t b -> m (Dynamic t c)) -> a -> Event t b -> Event t a -> m (Dynamic t c)


  --combine <- button "+" >>= (transformedPatternWidget (UntransformedPattern specPat) never <$) 
  combine <- button "+" 
  isCombined <- toggle False combine --  >>= combineDyn (\del com -> if del then False else com)

  --flippableWidget :: MonadWidget t m => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)

  let nextBuilder = do
      c <- patternCombinatorDropDown Merge never >>= mapDyn fst 
      --mapDyn show c >>= dynText
      (v,e) <- transformedPatternWidget (UntransformedPattern iSpecPat) never >>= splitDyn
      combineDyn (,) v e >>= combineDyn (\comb (x,y)-> (x,y,comb)) c

  nextWidget <- liftM joinDyn $ flippableWidget (return $ constDyn (EmptyTransformedPattern,never,Merge)) nextBuilder False (updated isCombined)
  --val <- flippableWidget nextWidget regWidget False (updated deleted))
  combineVal <- combineDyn (\a (b,c,d)->(a,b,c,d)) isCombined nextWidget

  nextWidgetVal <- mapDyn (\(x,y,_)->(x,y)) nextWidget

  let resetEvent = tagDyn nextWidgetVal deleteButton

  val <- resettableWidget dropdownPatternWidget iSpecPat never resetEvent

  val' <- combineDyn (\(a,_) b->(a,b)) patTrans specPat
  pat <- combineDyn (\(tog,cVal,ev,comb) (pT,spV) -> if tog then (TransformedPattern pT (TransformedPattern (Combine spV comb) cVal),ev) else (TransformedPattern pT $ UntransformedPattern spV,never)) combineVal val'
  mapDyn (show . fst) pat >>= dynText
  return pat
  where
    (transformer,iSpecPat) = case iTransPat of
      (TransformedPattern t (UntransformedPattern s)) -> (t,s)
      (UntransformedPattern s) -> (NoTransformer,s)
      (EmptyTransformedPattern) -> (NoTransformer,S $ Blank Inert Once)






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
