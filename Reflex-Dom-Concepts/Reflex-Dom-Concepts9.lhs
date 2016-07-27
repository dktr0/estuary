> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2


> data Simple = One | Two | Three deriving (Show)
> data SimpleWidgetRequest = Set Simple | Flash

> data SimpleWidgetEvent k = DeleteMe k deriving (Show)
> type Multiple = [Simple]


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


> requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple,Event t (SimpleWidgetEvent k)))
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


> growAndShrinkWidget' :: MonadWidget t m => m (Dynamic t Multiple)
> growAndShrinkWidget' = el "div" $ mdo
>   growEvent <- (return . (fmap (\k -> singleton k (Just One)))) =<< countEvent =<< button "Grow"
>   flashButton <- liftM (Flash <$) $ button "Flash"
>   let flashEvent = attachWith (\a b -> fromList (zip a (repeat b))) (current activeKeys) flashButton
>   let initialMap = empty :: Map Int Simple
>   let updateEvents = mergeWith (union) ([growEvent]++[deleteEvents'])
>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap updateEvents flashEvent requestableSimpleWidget -- m Dynamic t( Map k (Simple, SimpleWidgetEvent k))
>   values <- forDyn widgets (Prelude.map (fst) . elems)
>   events <- forDyn widgets (Prelude.map (snd) .elems)
>   deleteEvents <- forDyn events (fmap (fmap (\(DeleteMe k)-> singleton k Nothing)))
>   let deleteEvents' = switch $ fmap (mergeWith (union)) $ current deleteEvents -- Behaviour [Event Map ...]
>   activeKeys <- forDyn widgets (keys)
>   display values
>   return values


> main = mainWidget $ growAndShrinkWidget' >>= display

> countEvent :: (MonadWidget t m, Num k, Enum k) => Event t a -> m (Event t k)
> countEvent = zipListWithEvent (\a _ -> a) [0..]
