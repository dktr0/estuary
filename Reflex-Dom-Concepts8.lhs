> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2

> data Simple = One | Two | Three deriving (Show)
> type Multiple = [Simple]

Below we are refactoring listWithKeyShallowDiff from Reflex.Dom for the sake
of understanding. In a subsequent step we will make a variation on this definition
to separate the type of a signalling/requesting event for child widgets from the
value of interest represented by that widget.

> listWithKeyShallowDiff' :: (Ord k, MonadWidget t m) =>
>   Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
> listWithKeyShallowDiff' initial changes mkChild = do
>
>   let changesNoNothings = fmap (mapMaybe id) changes      -- Event (Map k v)
>   let selector = fanMap $ changesNoNothings               -- EventSelector (Const2 k v)
>   let mkChild' k v = mkChild k v $ select selector $ Const2 k

>   let changesNoValues = fmap (fmap (() <$)) changes       -- Event (Map k (Maybe () ))
>   let initialNoValues = () <$ initial                     -- Map k ()
>   stillExisting <- foldDyn (flip applyMap) initialNoValues changesNoValues -- m (Dynamic (Map k ()))  -- map of still-existing values

>   let relevantDiff diff _ = case diff of
>         Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
>         Just _ -> Nothing -- We don't want to let spurious re-creations of items through
>   let f = flip (differenceWith relevantDiff)

differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
when first and second map have an identical key, the provided function
determines what value - if it returns Nothing the entry is dropped, and if it returns Just x the entry is set to x

relevantDiff :: Maybe a -> b -> Maybe (Maybe a)
ignores second argument, if first argument is nothing returns Just Nothing, otherwise returns Nothing

differenceWith relevantDiff :: Map k (Maybe a) -> Map k b -> Map k (Maybe a)
flip (differenceWith relevantDiff) :: Map k b -> Map k (Maybe a) ->  Map k (Maybe a)
given a map over b and a map over maybe a, when there are matching keys,
if the value from a is Nothing store a Nothing (so that entry is deleted)
otherwise drop the entry (so as to not trigger a rebuild of that element in listHoldWithKey)

>   let beh = current stillExisting                           -- Behaviour (Map k ())
>   let evt = changes                                         -- Event (Map k (Maybe v))
>   let c = attachWith f beh evt                              -- Event
>   listHoldWithKey initial c mkChild'

attachWith :: Reflex t => (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
listHoldWithKey :: Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))

> listWithChildEvents :: (Ord k, MonadWidget t m)
>    => Map k v                        -- an ordered map of initial values
>    -> Event t (Map k (Maybe v))      -- construction events (add/replace/delete)
>    -> Event t (Map k r)              -- events delivered to child widgets
>    -> (k -> v -> Event t r -> m a)   -- function to make a widget given key, value and request event
>    -> m (Dynamic t (Map k a))
>
> listWithChildEvents initial cEvents rEvents mkChild = do
>   let selector = fanMap $ rEvents               -- EventSelector (Const2 k r)
>   let mkChild' k v = mkChild k v $ select selector $ Const2 k
>   let changesNoValues = fmap (fmap (() <$)) cEvents       -- Event (Map k (Maybe () ))
>   let initialNoValues = () <$ initial                     -- Map k ()
>   stillExisting <- foldDyn (flip applyMap) initialNoValues changesNoValues -- m (Dynamic (Map k ()))  -- map of still-existing values
>   let relevantDiff diff _ = case diff of
>         Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
>         Just _ -> Nothing -- We don't want to let spurious re-creations of items through
>   let f = flip (differenceWith relevantDiff)
>   let beh = current stillExisting                           -- Behaviour (Map k ())
>   let evt = cEvents                                         -- Event (Map k (Maybe v))
>   let c = attachWith f beh evt                              -- Event
>   listHoldWithKey initial c mkChild'

attachWith :: Reflex t => (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
listHoldWithKey :: Map k v -> Event t (Map k (Maybe v)) -> (k -

> data SimpleWidgetRequest = Set Simple | Flash

> requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t Simple)
> requestableSimpleWidget key initialValue signal = do
>   let flashEvent = fforMaybe signal g
>   flashToggle <- toggle True flashEvent
>   attr <- forDyn flashToggle h
>   let buttons = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   buttons' <- elDynAttr "div" attr buttons
>   let setEvent = fforMaybe signal f
>   value <- holdDyn initialValue (leftmost (buttons'++[setEvent]))
>   display value
>   return value
>   where f (Set x) = Just x
>         f _ = Nothing
>         g (Flash) = Just ()
>         g _ = Nothing
>         h True = singleton "style" "background-color: red; border: 3px solid black"
>         h False = singleton "style" "background-color: green; border: 3px solid black"
>
> growAndShrinkWidget' :: MonadWidget t m => m (Dynamic t Multiple)
> growAndShrinkWidget' = el "div" $ mdo
>   growEvent <- (return . (fmap (\k -> singleton k (Just One)))) =<< countEvent =<< button "Grow"
>   flashButton <- liftM (Flash <$) $ button "Flash"
>   activeKeys <- forDyn widgets (keys)
>   let activeKeys' = current activeKeys
>   let flashEvent = attachWith (\a b -> fromList (zip a (repeat b))) activeKeys' flashButton
>   let initialMap = empty :: Map Int Simple
>   lwce <- listWithChildEvents initialMap growEvent flashEvent requestableSimpleWidget
>   let widgets = joinDynThroughMap lwce
>   -- m (Dynamic (Map k Simple))
>   values <- forDyn widgets (elems)
>   display values
>   return values

> main = mainWidget $ growAndShrinkWidget' >>= display

> countEvent :: (MonadWidget t m, Num k, Enum k) => Event t a -> m (Event t k)
> countEvent = zipListWithEvent (\a _ -> a) [0..]
