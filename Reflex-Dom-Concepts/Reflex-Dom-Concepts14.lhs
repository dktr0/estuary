***This can probably be removed, didn't end up continuing with this direction***


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

> data Simple = One | Two | Three deriving (Show, Eq)
> type Multiple = [Simple]
> data WidgetEvent k = DeleteMe k | MakeSimple k deriving (Show)

> data Misc = Add deriving (Show)
> type Hetero = Either Simple Misc
> data WidgetRequest = Disable | MakeAllSimple | Set Simple | Flash deriving (Eq)


> listWithChildEvents :: (Ord k, MonadWidget t m)
>    => Map k v                        -- an ordered map of initial values
>    -> Event t (Map k (Maybe v))      -- construction events (add/replace/delete)
>    -> Event t (Map k WidgetRequest)              -- events delivered to child widgets
>    -> (k -> v -> Event t WidgetRequest -> m a)   -- function to make a widget given key, value and request event
>    -> m (Dynamic t (Map k a))
>
> listWithChildEvents initial cEvents rEvents mkChild = do
>   let selector = fanMap $ rEvents               -- EventSelector (Const2 k r)
>   let mkChild' k v = mkChild k v $ select selector $ Const2 k
>   let changesNoValues = fmap (fmap (() <$)) cEvents       -- Event (Map k (Maybe () ))- ex. Event t [(1,Just ()),(2,Nothing), (3, Just ())]
>   let initialNoValues = () <$ initial                     -- Map k ()
>   stillExisting <- foldDyn (flip applyMap) initialNoValues changesNoValues -- m (Dynamic (Map k ()))  -- map of still-existing values
>   let relevantDiff diff _ = case diff of
>         Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
>  --       Just _ -> Nothing -- We don't want to let spurious re-creations of items through
>         Just a -> Just (Just a)

@CHANGED^ to get '+' buttons working... Otherwise a creation (cEvent) event which would be called to change a "+" to
 a simple widget would not have any effect because the "+" widget would be "stillExisting" (I think)

>   let f = flip (differenceWith relevantDiff)
>   let beh = current stillExisting                           -- Behaviour (Map k ())
>   let evt = cEvents                                         -- Event (Map k (Maybe v))
>   let c = attachWith f beh evt                              -- Event
>   listHoldWithKey initial c mkChild'

^above appears to have same effect as just:

> listWithChildEvents' initial cEvents rEvents mkChild = do
>   let selector = fanMap $ rEvents               -- EventSelector (Const2 k r)
>   let mkChild' k v = mkChild k v $ select selector $ Const2 k
>   listHoldWithKey initial cEvents mkChild'

...but less effiecient?^

> requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t WidgetRequest -> m (Dynamic t (Simple, Event t (WidgetEvent k)))
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


> builder :: (MonadWidget t m, Ord k)=> k -> Hetero -> Event t WidgetRequest -> m (Dynamic t (Maybe Simple,Event t (WidgetEvent k)))
> builder k (Left simp) e = do
>  -- let event = coincidence $ fmap (either (<$ e) (return $ (Set Three) <$ never)) e -- if e=Event Left, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   requestableSimpleWidget k simp e >>= mapDyn (\(a,b)-> (Just a, b))
> builder k (Right misc) e = do
>  -- let event = coincidence $ fmap (either (return $ Disable <$ never) (<$ e)) e -- if e=Event Right, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   miscWidget k e >>= mapDyn (\(a,b)-> (Nothing, b))

> miscWidget:: (Ord k, MonadWidget t m )=> k -> Event t WidgetRequest -> m (Dynamic t (Simple, Event t(WidgetEvent k)))
> miscWidget key e = el "div" $ do
>   let request = fmap f e
>   clickButton <- button "+"
>   plusButton <- toggle True clickButton -- Event t (m (dynamic t (,)))
>   deleteButton <- liftM (DeleteMe key <$) $ button "-"
>   let events = leftmost [deleteButton, request]

Having trouble getting the last bit to compile, here's my thought:
When the button is pressed, the 'k' in the lamda becomes False,
and a becomes a requestableSimpleWidget with the same key and event stream.
Since the result of the lambda is then wrapped in a Dyn again, joinDyn is needed somewhere.

>   a <- forDyn plusButton (\k-> if k then constDyn (One,deleteButton) else requestableSimpleWidget key One e) -- Dyn m dyn t (,)?
>   return $ joinDyn a
>   where
>     f (MakeAllSimple) = MakeSimple key




> growAndShrinkWidget' :: MonadWidget t m => m (Dynamic t [Maybe Simple])
> growAndShrinkWidget' = el "div" $ mdo
>   let initialMap = empty :: Map Int Hetero
>   makeSimpleWidget <- liftM (fmap (=:(Just(Left One))) . (tagDyn maxKey)) $ button "Add SimpleWidget"
>   makeMiscWidget <- liftM (fmap (=:(Just(Right Add))) . (tagDyn maxKey)) $ button "Add MiscWidget"
>   let growEvents = mergeWith makeMap [makeMiscWidget, makeSimpleWidget]
>   let updateEvent = mergeWith union [growEvents, deleteEvents']
>   setTwoEvent <- liftM (attachWith (\a _-> fromList (zip a (repeat $ Left (Set Two)))) (current activeKeys) ) $ button "Set Two"
>   disableEvent <- liftM (attachWith (\a _-> fromList (zip a (repeat $ Right MakeAllSimple))) (current activeKeys) ) $ button "Make All SimpleWidgets"
>   let parentEvents = leftmost [setTwoEvent,disableEvent]
>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents' initialMap updateEvent parentEvents builder --MonadWidget t m => m (Dynamic t( Map k (Maybe Simple,Event t(SimpleWidgetEvent k))))
>   (values,events) <- forDyn widgets (unzip . elems) >>=splitDyn
>   -- events::Dynamic t [Event t (SimpleWidgetEvent k)]
>   deleteEvents <- forDyn events (fmap (fmap filterEvents)) -- m (Dynamic t [Event t (Map k Nothing...)])
>   let deleteEvents' = switch $ fmap (mergeWith (union)) $ current deleteEvents -- Behaviour [Event Map ...]
>   activeKeys <- forDyn widgets keys
>   maxKey <-  forDyn activeKeys (\k-> if k==[] then 0 else (maximum k)+1)
>   el "div" $ do
>     text "keys "
>     display activeKeys
>     el "div" $ return values
>   where
>     filterEvents (DeleteMe k) = k=:Nothing
>     filterEvents (MakeSimple k) =  fromList $ zip [k, k] [Nothing,(Just (Left One))]

makeMap should assign unique keys to two widgets when they're made at the same time, giving parameter 'a' the lower key

> makeMap::Map Int (Maybe Hetero) -> Map Int (Maybe Hetero) -> Map Int (Maybe Hetero)
> makeMap a b = union a $ fromList [(bKey+1,bVal)]
>   where (bKey,bVal) = elemAt 0 b

> main = mainWidget $ growAndShrinkWidget'>>=display
