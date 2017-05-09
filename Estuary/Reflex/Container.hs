{-# LANGUAGE RecursiveDo #-}
module Estuary.Reflex.Container where

-- The Estuary.Reflex namespace is for definitions that extend Reflex or
-- Reflex.Dom in ways that do not involve types specific to Estuary (or Tidal).
-- For example, as in this module, new container widgets for arbitrary widgets.

import Reflex.Dom
import GHCJS.DOM.EventM
import Data.Map
import Data.Bool
import Reflex
--import Estuary.Widgets.Generic -- for (EditSignal a)
import Estuary.Reflex.Utility
import Data.Functor.Misc -- For Const2
import Control.Monad
import Data.Maybe


widgetMap :: (MonadWidget t m,Ord k) => Map k (m a) -> Event t (Map k (m a)) -> m (Dynamic t (Map k a))
widgetMap iMap rebuild = do
  let iWidget = sequence $ elems iMap -- :: m [a]
  let rebuild' = fmap (sequence . elems) rebuild -- :: Event t (m [a])
  widgets <- widgetHold iWidget rebuild' -- :: m (Dynamic t [a])
  keys <- holdDyn (keys iMap) (fmap keys rebuild) -- :: m (Dynamic t [k])
  combineDyn (\a b -> fromList $ zip a b) keys widgets

container' :: (Ord k, Num k, MonadWidget t m)
  => (v -> m a) -- a builder function from
  -> Map k v -- an initial map of values
  -> Event t (Map k (Construction v)) -- construction events
  -> m (Dynamic t (Map k a))

container' build iMap cEvents = do
  let iMap' = fmap build iMap
  newMap <- foldDyn (\a b -> applyConstructionMap b a) iMap cEvents
  let newMap' = fmap (fmap build) (updated newMap)
  widgetMap iMap' newMap'


data Construction a = Insert a | Replace a | Delete deriving (Show)

-- given a Map and a Construction operation at a specifed key, return the new map

applyConstruction :: (Num k, Ord k) => Map k a -> (k,Construction a) -> Map k a
applyConstruction m (k,Replace a) = insert k a m
applyConstruction m (k,Delete) = delete k m
applyConstruction m (k,Insert a) = insert k a m'
  where m' = mapKeys (f) m
        f x | member k m = if (x>=k) then (x + 1) else x
            | otherwise = x

-- given a Map and another map of Construction operations, return the resulting map

applyConstructionMap :: (Num k, Ord k) => Map k a -> Map k (Construction a) -> Map k a
applyConstructionMap oldMap cMap = foldlWithKey (\a k b -> applyConstruction a (k,b)) oldMap cMap


-- given a Map and another map of Construction operations, determine the complete
-- list of "construction" events as expected by the Reflex function listHoldWithKey

constructionDiff :: (Num k, Ord k, Eq v) => Map k v -> Map k (Construction v) -> Map k (Maybe v)
constructionDiff oldMap cMap = unions [deletions,additions,changes]
  where newMap = applyConstructionMap oldMap cMap
        deletions = fmap (const Nothing) $ difference oldMap newMap -- keys only in oldMap are deletions
        additions = fmap (Just) $ difference newMap oldMap -- keys only in newMap are additions
        changes = fmap (Just) $ intersection newMap $ Data.Map.filter (id) $ intersectionWith (/=) oldMap newMap

container :: (Ord k, Num k, Show k, Eq v, Show v, MonadWidget t m)
   => Map k v                                -- a map of initial values
   -> Event t (Map k (Construction v))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                      -- signaling events to be delivered to child widgets
   -> (v -> Event t w -> m (Dynamic t (v,Event t x)))                -- function to make a widget given initial value and signaling event
   -> m ( (Dynamic t (Map k v)) , Event t (Map k x) )

container initialValue cEvents rEvents mkChild = mdo
  let cEventsIn = traceEvent "cEventsIn" cEvents
  let existingMap' = traceDyn "existingMap'" $ values
  let cEvents' = traceEvent "cEvents'" $ attachDynWith (constructionDiff) existingMap' cEventsIn
  let selector = fanMap rEvents
  let mkChild' k v = mkChild v $ select selector $ (Const2 k)
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValue cEvents' mkChild' -- Dynamic t (Map k (v,Event t x))
  values <- mapDyn (fmap (fst)) widgets
  events <- liftM (switchPromptlyDyn) $ mapDyn (mergeMap . fmap (snd)) widgets
  return (values,events)


eitherContainer :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e)))  -- function to build widgets for type v (returning events of type e)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e)))  -- function to build widgets for type a (also returning events of type e)
   -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) )

eitherContainer initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = mdo
  let cEvents' = attachDynWith (constructionDiff) values cEvents
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValues cEvents' mkChild
  values <- mapDyn (fmap (fst)) widgets
  events <- liftM (switchPromptlyDyn) $ mapDyn (mergeMap . fmap (snd)) widgets
  return (values,events)
  where
    mkChild k (Left x) = buildLeft x (select (fanMap eventsToLeft) (Const2 k)) >>= mapDyn (\(v,e)->(Left v,e))
    mkChild k (Right x) = buildRight x (select (fanMap eventsToRight) (Const2 k)) >>= mapDyn (\(a,e)->(Right a,e))


-- for widgets returning a 3rd event channel (for hints)
eitherContainer4 :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e,Event t e1)))  -- function to build widgets for type v (returning events of type e)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e,Event t e1)))  -- function to build widgets for type a (also returning events of type e)
   -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) , Event t e1 )

eitherContainer4 initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = mdo
  let cEvents' = attachDynWith (constructionDiff) values cEvents
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValues cEvents' mkChild -- m (Dynamic t (Map k a))
  values <- mapDyn (fmap (\(a,_,_)->a)) widgets
  events <- liftM (switchPromptlyDyn) $ mapDyn (mergeMap . fmap ((\(_,b,_)->b))) widgets
  events2 <- liftM (switchPromptlyDyn) $ mapDyn (leftmost . elems . fmap ((\(_,_,c)->c))) widgets -- @ may drop some messages if multiple hints coincide...
  return (values,events,events2)
  where
    mkChild k (Left x) = buildLeft x (select (fanMap eventsToLeft) (Const2 k)) >>= mapDyn (\(v,e,e2)->(Left v,e,e2))
    mkChild k (Right x) = buildRight x (select (fanMap eventsToRight) (Const2 k)) >>= mapDyn (\(a,e,e2)->(Right a,e,e2))



-- Same as eitherContainer' but children take Dynamic value too
-- primarily (/for now) used for updating children with liveness values
--eitherContainerLive :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
--   => Map k (Either v a)                               -- a map of initial values
--   -> Event t (Map k (Construction (Either v a)))       -- construction events (replace/insert/delete)
--   -> Dynamic t c
--   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
--   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
--   -> (Dynamic t c -> v -> Event t w -> m (Dynamic t (v,Event t e)))  -- function to build widgets for type v (returning events of type e)
--   -> (Dynamic t c -> a -> Event t b -> m (Dynamic t (a,Event t e)))  -- function to build widgets for type a (also returning events of type e)
--   -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) )

--eitherContainerLive initialValues cEvents liveness eventsToLeft eventsToRight buildLeft buildRight = mdo
--  let cEvents' = attachDynWith (constructionDiff) values cEvents
--  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValues cEvents' mkChild
--  values <- mapDyn (fmap (fst)) widgets
--  events <- liftM (switchPromptlyDyn) $ mapDyn (mergeMap . fmap (snd)) widgets
--  return (values,events)
--  where
--    mkChild k (Left x) = buildLeft liveness x (select (fanMap eventsToLeft) (Const2 k)) >>= mapDyn (\(v,e)->(Left v,e))
--    mkChild k (Right x) = buildRight liveness x (select (fanMap eventsToRight) (Const2 k)) >>= mapDyn (\(a,e)->(Right a,e))

{- ???
maybeContainer :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
   => Map k (Maybe v)                               -- a map of initial values
   -> Event t (Map k (Construction (Maybe v))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                             -- signaling events to be delivered to child widgets of type v
   -> (v -> Event t w -> m a)                       -- function to build widgets for Just values
   -> (Event t w -> m b)                            -- function to build widgets for Nothing values
   -> m (Dynamic t (Map k a))

widgetAndSpace ::
  => (v -> Event t w -> m a)
  -> m (Dynamic t a,)

-}


-- eitherContainer' is a variant of eitherContainer where the difference is that
-- only left values (not right) are included in the dynamic result:

eitherContainer' :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))      -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e)))  -- function to build widgets for type v (returning events of type x)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e)))  -- function to build widgets for type a (returning events of type c)
   -> m ( (Dynamic t (Map k v)) , Event t (Map k e) )
eitherContainer' initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = do
  (d,e) <- eitherContainer initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight
  d' <- mapDyn (Data.Map.mapMaybe (either (Just) (const Nothing))) d
  return (d',e)


eitherContainer'' :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e,Event t h)))  -- function to build widgets for type v (returning events of type e and f)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e,Event t h)))  -- function to build widgets for type a (also returning events of type e and f)
   -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e), Event t h)

eitherContainer'' initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = mdo
  let cEvents' = attachDynWith (constructionDiff) values cEvents
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValues cEvents' mkChild
  values <- mapDyn (fmap (\(x,_,_) -> x)) widgets
  eEvents <- liftM (switchPromptlyDyn) $ mapDyn (mergeMap . fmap (\(_,x,_) -> x)) widgets
  hEvents <- liftM (switchPromptlyDyn) $ mapDyn (leftmost . elems . fmap (\(_,_,x) -> x)) widgets
  return (values,eEvents,hEvents)
  where
    mkChild k (Left x) = buildLeft x (select (fanMap eventsToLeft) (Const2 k)) >>= mapDyn (\(v,e,f)->(Left v,e,f))
    mkChild k (Right x) = buildRight x (select (fanMap eventsToRight) (Const2 k)) >>= mapDyn (\(a,e,f)->(Right a,e,f))

eitherContainer''' :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
   => Map k (Either v a)                               -- a map of initial values
   -> Event t (Map k (Construction (Either v a)))      -- construction events (replace/insert/delete)
   -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
   -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
   -> (v -> Event t w -> m (Dynamic t (v,Event t e,Event t h)))  -- function to build widgets for type v (returning events of type e and f)
   -> (a -> Event t b -> m (Dynamic t (a,Event t e,Event t h)))  -- function to build widgets for type a (returning events of type e and f)
   -> m ( Dynamic t (Map k v) , Event t (Map k e), Event t h)
eitherContainer''' initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight = do
  (d,e,h) <- eitherContainer'' initialValues cEvents eventsToLeft eventsToRight buildLeft buildRight
  d' <- mapDyn (Data.Map.mapMaybe (either (Just) (const Nothing))) d
  return (d',e,h)


{-
eitherContainer'' :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
  => Map k (Either v a)
  -> Event t (Map k (Construction (Either v a)))
  -> (v -> m (Dynamic t (v,Event t e)))
  -> (a -> m (Event t c))
  -> m ( Dynamic t (Map k v), Event t (Map k e), Event t (Map k c) )

eitherContainer'' i cEvents lBuild rBuild = mdo
  widgets <- listHoldWithKey i cEvents? build -- m (Dynamic t (Map k (Either (Dynamic t (v,Event t e)) (Event t c))))
  values <- liftM joinDynThroughMap $ forDyn widgets (fmapFstIntoMap  . filterMapForOnlyLeftElements)
  lEvents <- forDyn widgets (fmapSndIntoMap  .  filterMapForOnlyLeftElements)
  rEvents <- forDyn widgets

  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey i cEvents' build
  values <- mapDyn (fmap (fst)) widgets
  events <- liftM (switchPromptlyDyn) $ mapDyn (mergeMap . fmap (snd)) widgets
  return (values,lEvents,rEvents)
  where
    build _ (Left v) = lBuild v >>= mapDyn (\(v,e)-> Left (v,e))
    build _ (Right a) = rBuild a >>= mapDyn (\c-> Right c)
-}

eitherWidget :: (MonadWidget t m)
  => (a -> Event t c -> m (Dynamic t (a,Event t d)))
  -> (b -> Event t c -> m (Dynamic t (b,Event t d)))
  -> Either a b -> Event t c -> m (Dynamic t ((Either a b),Event t d))

eitherWidget buildA buildB iValue c = either buildA' buildB' iValue
  where
    buildA' a = buildA a c >>= mapDyn (\(x,d) -> (Left x,d))
    buildB' b = buildB b c >>= mapDyn (\(x,d) -> (Right x,d))

wfor :: (MonadWidget t m) => [a] -> (a -> m (Dynamic t b)) -> m (Dynamic t [b])
wfor iVals mkChild = do
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey iMap never mkChild' -- m (Dynamic t [b]))
  mapDyn elems widgets
  where
    iMap = fromList $ zip [(0::Int)..] iVals
    mkChild' _ a = mkChild a

wmap :: (MonadWidget t m) => (a -> m (Dynamic t b)) -> [a] -> m (Dynamic t [b])
wmap = flip wfor

-- resettableWidget: given a standard Estuary widget function, produce a
-- variant with a reset Event of the same main type
-- resettableWidget :: (MonadWidget t m, Eq a)=> (a -> Event t () -> m (Dynamic t (a,Event t (EditSignal a)))) -> a -> Event t () -> Event t a -> m (Dynamic t (a,Event t (EditSignal a)))
-- resettableWidget :: (MonadWidget t m, Eq a)=> (a -> Event t (EditSignal a) -> m (Dynamic t (a,Event t (EditSignal a)))) -> a -> Event t (EditSignal a) -> Event t a -> m (Dynamic t (a,Event t (EditSignal a)))
-- resettableWidget widget i e reset = liftM (joinDyn) $ widgetHold (widget i e) $ fmap (\x -> widget x e) reset
-- after a merge conflict... believe the more general version below holds

resettableWidget :: MonadWidget t m => (a -> Event t b -> m (Dynamic t c)) -> a -> Event t b -> Event t a -> m (Dynamic t c)
resettableWidget f i e reset = liftM (joinDyn) $ widgetHold (f i e) $ fmap (\x -> f x e) reset

{-
makeResettableWidget ::  MonadWidget t m => (a -> Event t b -> m (Dynamic t (a,Event t (EditSignal a)))) -> a -> Event t b -> m (Dynamic t (a,Event t (EditSignal a)))
makeResettableWidget b i e = mdo
  val <- resettableWidget b i e rebuildEvents'
  rebuildEvents <- liftM (tagDyn val) $ liftM (switchPromptlyDyn) $ mapDyn (ffilter (==RebuildMe) . snd) val
  let rebuildEvents' = attachDynWith (\(a,_) _ -> a) val rebuildEvents
  otherEvents <- liftM (switchPromptlyDyn) $ mapDyn (ffilter (/=RebuildMe) . snd) val
  mapDyn (\(x,_) -> (x,otherEvents)) val
-}

popup :: MonadWidget t m => Event t (Maybe (m (Event t a))) -> m (Event t a)
popup buildEvents = do
  let buildEvents' = fmap (maybe (return never) id) buildEvents
  liftM (switchPromptlyDyn) $ widgetHold (return never) buildEvents'


-- the following three definitions are just an example of using 'popup' above to implement a popup menu
-- they should probably be moved to an examples folder sometime soon...

clickableWhiteSpace :: MonadWidget t m => m (Event t ())
clickableWhiteSpace = do
  (element,_) <- elAttr' "div" (singleton "class" "clickableWhiteSpace") $ text "clickableWhiteSpace"
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ (() <$) clickEv

flippableWidget :: MonadWidget t m => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)
flippableWidget b1 b2 i e = widgetHold (bool b1 b2 i) $ fmap (bool b1 b2) e
