> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2
> import qualified Data.Maybe

> import           GHCJS.Types as GHCJS
> import qualified GHCJS.DOM.Event  as GHCJS (IsEvent)
> import qualified GHCJS.DOM.Element as GHCJS
> import           GHCJS.DOM.EventM as GHCJS (preventDefault, stopPropagation, EventM)

> data Simple = One | Two | Three deriving (Show,Eq)
> type Multiple = [Simple]
> data SimpleWidgetRequest = Set Simple | Flash
> data WidgetEvent = DeleteMe | MakeSimple deriving (Show,Eq)

> data Misc = Add deriving (Show,Eq)
> type Hetero = Either Simple Misc
> data MiscRequest = Resize | Disable | MakeAllSimple deriving (Eq)

> data Construction a = Insert a | Replace a | Delete

Given a Construction operation, a position, and a Map, return the changes
necessary to update the map in accordance with operation.

> constructionDiff :: (Num k, Ord k) => k -> Construction a -> Map k a -> Map k (Maybe a)
> constructionDiff k (Replace x) m = singleton (k,Just x)
> constructionDiff k (Delete) m = singleton (k,Nothing)
> constructionDiff k (Insert x) m = union (singleton (k,Just x)) remainder
>   where remainder | not (member k m) = empty
>                   | otherwise = map (Just) . mapKeys (+ 1)  . partitionWithKey (\n _ -> n >= k) $ m
>   -- when Insert-ing, all keys greater than or equal to insert position go up by one
>   -- and are applied to Just constructor

> listWithChildEvents :: (Ord k, MonadWidget t m,Show k, Show v, Eq v)
>    => Map k v                        -- an ordered map of initial values
>    -> Event t (Map k (Construction v))
>    -> Event t (Map k w)              -- events delivered to child widgets
>    -> (k -> v -> Event t (Map k w) -> m a)   -- function to make a widget given key, value and request event
>    -> m (Dynamic t (Map k a))
>
> listWithChildEvents initial cEvents rEvents mkChild = do
>   let selector = fanMap $ rEvents               -- EventSelector (Const2 k r)
>   let mkChild' k v = mkChild k v $ select selector $ Const2 k
>   existing <- foldDyn constructionF initial cEvents
>   -- if Replace, make a straightforward add event
>   -- if Delete, make a straightforward delete event
>   -- if Insert and keys are not in use, make a straightforward add event
>   -- if Insert and keys are in use, shift elements with keys >= AND make straightforward add eventss
>   -- what if construction contains multiple  
>   listHoldWithKey initial cEvents mkChild'



> requestableSimpleWidget :: MonadWidget t m => Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t WidgetEvent))
> requestableSimpleWidget initialValue signal = do
>   let flashEvent = fforMaybe signal g
>   flashToggle <- toggle True flashEvent
>   attr <- forDyn flashToggle h
>   let buttons = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   buttons' <- elDynAttr "div" attr buttons
>   deleteButton <- liftM (DeleteMe <$) $ button "-"
>   let setEvent = fforMaybe signal f
>   value <- holdDyn initialValue (leftmost (buttons'++[setEvent]))
>   display value
>   forDyn value (\a-> (a,deleteButton))
>   where f (Set x) = Just x
>         f _ = Nothing
>         g (Flash) = Just ()
>         g _ = Nothing
>         h True = singleton "style" "background-color: red; border: 3px solid black"
>         h False = singleton "style" "background-color: green; border: 3px solid black"


> builder :: MonadWidget t m => Hetero  -> Event t SimpleWidgetRequest
>   -> m (Either (Dynamic t (Simple,Event t WidgetEvent)) (Event t WidgetEvent))
> builder (Left simp) e = do
>   x <- requestableSimpleWidget k simp e
>   return $ Left x
> builder k (Right misc) _ = do
>   x <- miscWidget
>   return $ Right x

> miscWidget:: MonadWidget t m => m (Event t WidgetEvent)
> miscWidget = el "div" $ do
>   plusButton <- liftM (MakeSimple <$) $ button "  +  "
>   deleteButton <- liftM (DeleteMe <$) $ button "-"
>   return $ leftmost [deleteButton, plusButton]

> growAndShrinkWidget' :: MonadWidget t m => m (Dynamic t [Simple])
> growAndShrinkWidget' = el "div" $ mdo
>   let initialMap = fromList [(0,Add)]

>   let blah = foldl (   ) -- Map k (Either Simple Misc)
>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap updateEvents never builder
>   -- MonadWidget t m => m (Dynamic t ( Map k ( (Either (Dynamic t (Simple,Event t WidgetEvent)) (Event t WidgetEvent))))
>   existing <- mapDyn (     )
>   valueMap <- joinDyn $ mapDyn (mapDyn (fst)  .  filter (isLeft)) widgets -- Dynamic (Map k Simple)
>   let f x = switchPromptlyDyn $ mapDyn (snd) x
>   eventMap <- switchPromptlyDyn $ mapDyn (map (either (f) (id))) widgets -- Event t (Map k WidgetEvent)

>   let deleteEvent = fmap ((Nothing <$) . filter (==DeleteMe)) eventMap

>   let makeSimple = fmap ( (() <$) . filter (==MakeSimple)) eventMap -- Event t (Map k ())
>   let makeSimpleF = fromList . concat . map (\k -> [(k,Just (Right Add)),(k+1),Just (Left One)]) . keys -- Map k () -> Map k (Either Simple Misc)
>   let makeSimple' = fmap (makeSimpleF) makeSimple -- Event t (Map k (Either Simple Misc))
>

>   let updateEvents = mergeWith union [deleteEvent,makeSimpleEvents]

widgets' <- forDyn widgets (Data.Map.map fst) -- dyn Map k (Maybe Simple)
widgets'' <- forDyn widgets' (Data.Map.map (\x->case x of (Just val)-> Just (Left val); otherwise->Just (Right Add)))

>   existing <-

>   let inserts = attachDynWith updateMap widgets'' insertEvent
>   activeKeys <- forDyn widgets keys
>   maxKey <-  forDyn activeKeys (\k-> if k==[] then 0 else (maximum k)+1)
>   el "div" $ do
>     text "keys "
>     display activeKeys
>     el "div" $ return values
>   where
>     applyEvents (DeleteMe k) = k=:Nothing
>     applyEvents (MakeSimple k) =  k=:Just (Left One)

This next function (g) takes a map that might contain a MakeSimple request
and translates it to a map maybe containing two construction events.

> insert' :: (Num k,Ord k) => Map k a -> k -> a -> Map k a
> insert' m pos a = insert pos a (union x y')
>   where (x,y) = partitionWithKey (\k a -> k < pos) m
>         y' = mapKeys f y
>         f | (member pos m) = (+ 1)
>           | otherwise = id


> g :: Ord k => Map k WidgetEvent -> Map k (Either Simple Misc)
> g x = union h i
>   where h = fmap ((Just (Left One) <$) . filter (==MakeSimple)) x
>         i = fromList (map (\a -> (a,Right Misc)) (keys h))

> j :: Ord k => Map k a -> Map k (Either Simple Misc) -> Map k (Maybe (Either Simple Misc))
> j statusQuo intentions =

> updateMap:: Map Int (Maybe Hetero) -> Map Int (Maybe Hetero) -> Map Int (Maybe Hetero)
> updateMap oldMap mapChanges = if Data.Map.null mapChanges then oldMap else updateMap (Data.Map.insert firstKey firstElem oldMap') (deleteMin mapChanges)
>   where (firstKey, firstElem) = findMin mapChanges
>         oldKeys = keys oldMap
>         keyMap x | x>=firstKey = x+1
>                  | otherwise = x
>         newKeys| elem firstKey oldKeys = Prelude.map keyMap oldKeys
>                  | otherwise = oldKeys
>         oldMap' = fromList $ zip newKeys (elems oldMap)

- in case where add button is turning into simple, don't want it to be x>=, just want x> in keyMap x

makeMap should assign unique keys to two widgets when they're made at the same time, giving parameter 'a' the lower key

> makeMap::Map Int (Maybe Hetero) -> Map Int (Maybe Hetero) -> Map Int (Maybe Hetero)
> makeMap a b = union a $ fromList [(bKey+1,bVal)]
>   where (bKey,bVal) = elemAt 0 b

> main = mainWidget $ growAndShrinkWidget'>>=display
