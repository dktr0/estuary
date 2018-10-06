{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.Sequencer where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM

import Control.Monad
import qualified Sound.Tidal.Context as Tidal
import Data.Map as M
import Data.Ratio

import Estuary.Tidal.Types
import Estuary.Types.Hint
import Estuary.Widgets.Generic -- for EditSignal... TODO move that


toPattern :: (Tidal.Parseable a, Show a, Tidal.Enumerable a) => GeneralPattern a -> Tidal.Pattern a
toPattern = Tidal.p . show

type Sequence a = Map a [Bool]

attachIndex:: [a] -> [(Int,a)]
attachIndex l = zip (take (length l) [0..]) l

toSequence :: (Ord a, Eq a,Tidal.Parseable a, Show a, Tidal.Enumerable a)=> GeneralPattern a -> Sequence a
toSequence pat = M.fromList $  fmap (\(v,s) -> (v, fmap (\x-> elem x s) $ take ((fromInteger maxDenom)::Int) [0..])) valList where
  events = fmap (\((a,_),(b,_),v)-> (a,b,v)) $ (\x-> (Tidal.arc x) (0,1) ) $ toPattern pat -- [(Rational(start),Rational(end),val)]
  maxDenom = maximum $ fmap (\(a,b,_)-> max (denominator a) (denominator b)) events -- represents # of sequencer steps
  pattern = fmap (\(a,_,v)-> ((numerator a * (div maxDenom $ denominator a)),v)) events  -- [(Int, Val)] scaling rationals over # of steps
  vals = fmap snd pattern -- represents just values
  valList = fmap (\v-> (v, fmap (fromIntegral .fst) $ Prelude.filter (\(_,v')-> v==v') pattern)) vals -- [(val, [Int])]

toGenPat:: Sequence a -> GeneralPattern a
toGenPat s = Layers (Live (M.elems groups,Once) L4) Inert where
  groups = M.mapWithKey toGroup s -- [GeneralPat]
  toGroup val isActives = Group (Live (fmap (toAtom val) isActives, Once) L4) Inert -- (v,[Bool]) -> [GeneralPat]
  toAtom val isActive = if isActive ttConfig_attributes .~  inputAttrs & textInputConfig_initialValue .~ (showNoQuotes iVal)

sequencer::(Ord a, MonadWidget t m, Show a, Eq a,Tidal.Parseable a, Tidal.Enumerable a) => GeneralPattern a -> Event t (EditSignal (Sequence a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
sequencer igp edits = elClass "table" "sequencer" $ do
  let es = fmap (fmap Just) $ fmapMaybe (\a -> case a of (ChangeValue a) -> Just a; otherwise -> Nothing) edits -- Event (Map a (Maybe [Bool]))
  let s = toSequence igp -- Map a [Bool]
  pb <- getPostBuild
  debug $ fmap (const asdf) s
  seqMatrix <- liftM joinDynThroughMap $ listWithKeyShallowDiff s es sequencerRow
  genPat <- mapDyn (toGenPat . fmap elems) seqMatrix
  mapDyn (\x-> (x, never, never)) genPat


  -- <- el "td" $ text $ growingTextInput $ def & textInputConfig_initialValue .~ (showNoQuotes label)


sequencerRow ::(MonadWidget t m, Show a) => a -> [Bool] -> Event t [Bool] -> m (Dynamic t (Map Int Bool))
sequencerRow label vals edits = elClass "tr" "sequencerRow" $ do
  let buttonIVals = M.fromList $ attachIndex vals
  el "td" $ text $ show label
  buttons <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals (fmap (fmap Just . M.fromList . attachIndex) edits) sequencerButton  -- Dyn (Map Int Bool)
  mapDyn (id) buttons


sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t Bool)
sequencerButton pos val edits = mdo
  (element,_) <- elDynAttr' "td" attrs $ return ()
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]
  debug $ updated isActive
  attrs <- mapDyn (\x-> singleton "class" $ if x then "sequencerButtonActived" else "sequencerButtonDeactivated") isActive
  return isActive
