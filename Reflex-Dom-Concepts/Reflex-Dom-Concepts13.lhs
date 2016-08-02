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
> data WidgetEvent k = DeleteMe k | MakeSimple k deriving (Show)

> data Misc = Add deriving (Show,Eq)
> type Hetero = Either Simple Misc
> data MiscRequest = Resize | Disable | MakeAllSimple deriving (Eq)

> listWithChildEvents :: (Ord k, MonadWidget t m,Show k, Show v, Eq v)
>    => Map k v                        -- an ordered map of initial values
>    -> Event t (Map k (Maybe v))      -- construction events (add/replace/delete)
>    -> Event t (Map k (Either SimpleWidgetRequest MiscRequest ))              -- events delivered to child widgets
>    -> (k -> v -> Event t (Either SimpleWidgetRequest MiscRequest) -> m a)   -- function to make a widget given key, value and request event
>    -> m (Dynamic t (Map k a))
> listWithChildEvents initial cEvents rEvents mkChild = do
>   let selector = fanMap $ rEvents               -- EventSelector (Const2 k r)
>   let mkChild' k v = mkChild k v $ select selector $ Const2 k
>   stillExisting <- foldDyn (flip applyMap) initial cEvents >>= mapDyn (Data.Map.map Just) -- m (Dynamic (Map k ()))  -- map of still-existing values
>   let relevantDiff diff other = case diff of
>         Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
>         Just a -> if diff/=other then Just (Just a) else Nothing --if the value at that key have changed then build the new widget, otherwise recreate what is already there
>   let f = flip (differenceWith relevantDiff)
>   let c = attachWith f (current stillExisting) cEvents
>   listHoldWithKey initial c mkChild'

> requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t (WidgetEvent k)))
> requestableSimpleWidget key initialValue signal = do
>   let flashEvent = fforMaybe signal g
>   flashToggle <- toggle True flashEvent
>   attr <- forDyn flashToggle h
>   let buttons = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   buttons' <- elDynAttr "div" attr buttons
>   deleteButton <- liftM (DeleteMe key <$) $ button "-"
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


> builder :: (MonadWidget t m, Ord k)=> k -> Hetero -> Event t (Either SimpleWidgetRequest MiscRequest) -> m (Dynamic t (Maybe Simple,Event t (WidgetEvent k)))
> builder k (Left simp) e = do
>   let event = coincidence $ fmap (either (<$ e) (return $ (Set Three) <$ never)) e -- if e=Event Left, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   requestableSimpleWidget k simp event >>= mapDyn (\(a,b)-> (Just a, b))
> builder k (Right misc) e = do
>   let event = coincidence $ fmap (either (return $ Disable <$ never) (<$ e)) e -- if e=Event Right, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   miscWidget k event >>= mapDyn (\(a,b)-> (Nothing, b))

> miscWidget:: (Ord k, MonadWidget t m )=> k -> Event t MiscRequest -> m (Dynamic t (Simple, Event t(WidgetEvent k)))
> miscWidget key e = el "div" $ do
>   let request = fmap f e
>   plusButton <- liftM (MakeSimple key <$) $ button "  +  "
>   deleteButton <- liftM (DeleteMe key <$) $ button "-"
>   let events = leftmost [deleteButton, plusButton, request]
>   return $ constDyn (One,events)
>   where
>     f (MakeAllSimple) = MakeSimple key

> growAndShrinkWidget' :: MonadWidget t m => m (Dynamic t [Maybe Simple])
> growAndShrinkWidget' = el "div" $ mdo
>   let initialMap = empty :: Map Int Hetero
>   makeSimpleWidget <- liftM (fmap (=:(Just(Left One))) . (tagDyn maxKey)) $ button "Add SimpleWidget"
>   makeMiscWidget <- liftM (fmap (=:(Just(Right Add))) . (tagDyn maxKey)) $ button "Add MiscWidget"
>   let growEvents = mergeWith makeMap [makeMiscWidget, makeSimpleWidget]
>   let updateEvent = mergeWith union [growEvents, childEvents']
>   setTwoEvent <- liftM (attachWith (\a _-> fromList (zip a (repeat $ Left (Set Two)))) (current activeKeys) ) $ button "Set Two"
>   makeAllSimpleEvent <- liftM (attachWith (\a _-> fromList (zip a (repeat $ Right MakeAllSimple))) (current activeKeys) ) $ button "Make All SimpleWidgets"
>   let parentEvents = leftmost [setTwoEvent,makeAllSimpleEvent]
>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap updateEvent parentEvents builder --MonadWidget t m => m (Dynamic t( Map k (Maybe Simple,Event t(SimpleWidgetEvent k))))
>   (values,events) <- forDyn widgets (unzip . elems) >>=splitDyn
>   -- events::Dynamic t [Event t (SimpleWidgetEvent k)]
>   childEvents <- forDyn events (fmap (fmap applyEvents)) -- m (Dynamic t [Event t (Map k Nothing...)])
>   let childEvents' = switch $ fmap (mergeWith (union)) $ current childEvents -- Behaviour [Event Map ...]
>   activeKeys <- forDyn widgets keys
>   maxKey <-  forDyn activeKeys (\k-> if k==[] then 0 else (maximum k)+1)
>   el "div" $ do
>     text "keys "
>     display activeKeys
>     el "div" $ return values
>   where
>     applyEvents (DeleteMe k) = k=:Nothing
>     applyEvents (MakeSimple k) =  k=:Just (Left One)


makeMap should assign unique keys to two widgets when they're made at the same time, giving parameter 'a' the lower key

> makeMap::Map Int (Maybe Hetero) -> Map Int (Maybe Hetero) -> Map Int (Maybe Hetero)
> makeMap a b = union a $ fromList [(bKey+1,bVal)]
>   where (bKey,bVal) = elemAt 0 b

> main = mainWidget $ growAndShrinkWidget'>>=display
