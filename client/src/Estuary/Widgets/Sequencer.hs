{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.Sequencer where

{-
import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM

import Control.Monad
import Data.Map as M
import Data.Ratio
import Safe.Foldable (maximumMay)
import Text.Read (readMaybe)

import Estuary.Tidal.Types
import Estuary.Tidal.ParamPatternable (parseBP')
import Estuary.Types.Hint
import Estuary.Widgets.Generic -- for EditSignal... TODO move that
import Estuary.Types.Live


import qualified Sound.Tidal.Context as T
import qualified Text.ParserCombinators.Parsec as P

toPattern :: (T.Parseable a, Show a, T.Enumerable a) => GeneralPattern a -> T.Pattern a
toPattern = parseBP' . show


toPatternMaybe :: (T.Parseable a, Show a, T.Enumerable a) => GeneralPattern a -> T.Pattern (Maybe a)
toPatternMaybe = toPat' . T.parseTPat . show


type Sequence a = Map Int (Maybe a,[Bool])

attachIndex:: [a] -> [(Int,a)]
attachIndex l = zip (take (length l) [0..]) l



toPat' :: T.Enumerable a => T.TPat a -> T.Pattern (Maybe a)
toPat' a = case a of
  T.TPat_Atom x -> pure  (Just x)
  T.TPat_Density t x -> T.density (T.toPat t) $ toPat' x
  T.TPat_Slow t x -> T.slow (T.toPat t) $ toPat' x
  T.TPat_Zoom arc x -> T.zoom arc $ toPat' x
  T.TPat_DegradeBy amt x -> T._degradeBy amt $ toPat' x
  T.TPat_Silence -> pure Nothing
  T.TPat_Cat xs -> T.fastcat $ Prelude.map toPat' xs
  T.TPat_TimeCat xs -> T.timeCat $ Prelude.map (\(n, p) -> (toRational n, toPat' p)) $ T.durations xs
  T.TPat_Overlay x0 x1 -> T.overlay (toPat' x0) (toPat' x1)
  T.TPat_ShiftL t x -> t `T.rotL` toPat' x
  -- T.TPat_pE n k s thing -> T.unwrap $ _eoff <$> toPat' n <*> toPat' k <*> toPat' s <*> pure (toPat' thing)
  T.TPat_Foot -> error "Can't happen, feet (.'s) only used internally.."
  T.TPat_EnumFromTo a b -> fmap Just $ T.unwrap $ T.fromTo <$> (T.toPat a) <*> (T.toPat b)
  -- T.TPat_EnumFromThenTo a b c -> T.unwrap $ T.fromThenTo <$> (toPat' a) <*> (toPat' b) <*> (toPat' c)





toSequence :: (Ord a, Eq a,T.Parseable a, Show a, T.Enumerable a)=> Int -> GeneralPattern a -> Sequence a
toSequence iIndex pat = M.fromList $ zip [iIndex..] $ toList $ fromList $ fmap (\(v,s) -> (v, fmap (\x-> elem x s) $ take ((fromInteger maxDenom)::Int) [0..])) valList
  where
    events = fmap (\((a,_),(b,_),v)-> (a,b,v)) $ (\x-> (T.arc x) (0,1) ) $ toPatternMaybe pat -- [(Rational(start),Rational(end),val)]
    maxDenom = maximum $ fmap (\(a,b,_)-> max (denominator a) (denominator b)) events -- represents # of sequencer steps
    pattern = fmap (\(a,_,v)-> ((numerator a * (div maxDenom $ denominator a)),v)) events  -- [(Int, Val)] scaling rationals over # of steps
    vals = fmap snd pattern -- represents just values
    valList = fmap (\v-> (v, fmap (fromIntegral .fst) $ Prelude.filter (\(_,v')-> v==v') pattern)) vals -- [(val, [Int])]

toGenPat:: Sequence a -> GeneralPattern a
toGenPat s = Layers (Live (M.elems groups,Once) L4) Inert where
  groups = fmap toGroup s -- Map a GeneralPat
  toGroup (val,isActives) = Group (Live (fmap (toAtom val) isActives, Once) L4) Inert -- (v,[Bool]) -> [GeneralPat]
  toAtom val isActive = if isActive then maybe (Blank Inert) (\x-> Atom x Inert Once) val else Blank Inert

rowToGenPat:: (Maybe a,[Bool]) -> GeneralPattern a
rowToGenPat (val,pos) = Group (Live (fmap toAtom pos,Once) L4) Inert
  where
    toAtom b = if b then maybe (Blank Inert) (\x-> Atom x Inert Once) val else Blank Inert


rowToGenPat'::Read a => (String,[Bool]) -> GeneralPattern a
rowToGenPat' (val,pos) = Group (Live (fmap toAtom pos,Once) L4) Inert
  where
    toAtom b = if b then parsed else Blank Inert
    parsed = maybe (maybe (Blank Inert) f $ readMaybe $ "\""++val++"\"") f $ readMaybe val
    f x = Atom x Inert Once

{-
sequencer' ::MonadWidget t m => [(String, [Bool])] -> Event t [(String,[Bool])] -> m (Dynamic t ([(String,[Bool])], Event t [(String,[Bool])], Event t Hint))
sequencer' i update = do
  let iVal = toGenPat $ fromList $ zip [0..] $ fmap (\(x,y)->(Just x,y)) i -- GeneralPattern String
  let e = fmap (\(x,y) -> (Just x,y)) update
  v <- sequencer Nothing iVal e
  mapDyn (\(v,ev,h) -> (toSequence 0 v, fmap (toSequence 0) $ getChangeValues ev,h)) v
  where
    getChangeValues eve = fmapMaybe $ fmap (\x-> case x of ChangeValue a -> Just a; otherwise-> Nothing) eve
-}

sequencer::(Read a, Ord a, MonadWidget t m, Show a, Eq a,T.Parseable a, T.Enumerable a) => Maybe a -> GeneralPattern a -> Event t (EditSignal (Sequence a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
sequencer dflt igp edits = elClass "table" "sequencer" $ mdo
  let parentEvents = fmap (fmap Just) $ fmapMaybe (\a -> case a of (ChangeValue a) -> Just a; otherwise -> Nothing) edits -- Event (Map a (Maybe [Bool]))
  let es = leftmost [parentEvents,deleteChildrenEvent, newRow] -- Event t (Map Int (Maybe (a,[Bool])))
  let s = toSequence 0 igp -- Map Int (a,[Bool])
  let seqLen = maximum $ elems $ fmap (length . snd) s
  vals <- liftM joinDynThroughMap $ listWithKeyShallowDiff s es (const sequencerRow) -- Dynamic (Map Int (GeneralPattern, Event t Edit))
  maxKey <- mapDyn (maybe 0 id . maximumMay . keys) vals
  let emptyGroup = Group (Live (take seqLen $ repeat $ Blank Inert,Once) L4) Inert
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> fmap Just $ singleton (k+1) (dflt,take seqLen $ repeat False)) (current maxKey) plusButton
  gps <- mapDyn (fmap fst) vals -- Dyn (Map k (GenPat))
  genPat <- mapDyn (\x-> Layers (Live (elems x,Once) L4) Inert) gps
  -- TODO - Ouch:
  sigs <- mapDyn (fmap snd) vals -- Dynamic t (Map k (Event t (EditSignal k)))
  sigs' <- mapDyn (fmap (\(k,ev)-> fmap (\v->(k,v)) ev) . toList) sigs -- Dynamic t ([Event t (a,EditSig)])
  sigs'' <- mapDyn (leftmost) sigs' -- Event t (a,EditSignal)
  let deleteChildrenEvent = fmap (\(a,_)-> singleton a Nothing) $ ffilter ((==DeleteMe) . snd) $ switch $ current sigs''
  mapDyn (\x-> (x, never, never)) genPat


typeCheck::  Maybe a -> Maybe a -> Maybe a
typeCheck _ a = a

sequencerRow ::(MonadWidget t m, Show a, Ord a, Read a) => (Maybe a,[Bool]) -> Event t (Maybe a,[Bool]) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal a)))
sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
  let buttonIVals = M.fromList $ attachIndex vals
  deleteMe <- clickableTdClass (constDyn " - ") (constDyn "delete") DeleteMe
  rowInput <- el "td" $ growingTextInput $ def & textInputConfig_initialValue .~ (maybe "~" showNoQuotes iVal)
  buttons <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals (fmap (fmap Just . M.fromList . attachIndex . snd) edits) sequencerButton  -- Dyn (Map Int Bool)
  genPat <- combineDyn (\val positions-> rowToGenPat' (val,elems positions)) (_textInput_value rowInput) buttons
  mapDyn (\x->(x,deleteMe)) genPat



sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t Bool)
sequencerButton pos val edits = mdo
  (element,_) <- elDynAttr' "td" attrs $ return ()
  clickEv <- wrapDomEvent (_el_element element) (onEventName Mousedown) (return ())
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]
  debug $ updated isActive
  attrs <- mapDyn (\x-> singleton "class" $ if x then "sequencerButtonActived" else "sequencerButtonDeactivated") isActive
  return isActive
-}
