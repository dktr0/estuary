> {-# LANGUAGE RecursiveDo #-}
> module Growing where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2
> import Multiple

Next Steps:

1. widgets that can send signals to their parents:

> data SimpleWidgetEvent k = DeleteMe k deriving (Show)
>
> deletableWidget :: (Ord k, MonadWidget t m) =>
>   k -> Simple -> Event t Simple -> m (Dynamic t Simple, Event t (SimpleWidgetEvent k)
> deletableWidget k i _ = el "div" $ do
>   deleteEvent <- liftM (DeleteMe k <$) $ button "-"
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$)) (button (show x))
>   value <- holdDyn i (leftmost buttons)
>   return (value,deleteEvent)
>
> deletableWidget :: MonadWidget t m =>
>   k => Simple -> Event t Simple -> m (Dynamic t (Simple, Event t (SimpleWidgetEvent Int)))
> deletableWidget' k i _ = el "div" $ do
>   deleteEvent0 <- liftM (DeleteMe k <$) $ button "-"
>   let deleteEvent = traceEvent "deleteInternal" deleteEvent0
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn i (leftmost buttons)
>   display value
>   forDyn value (\a -> (a,deleteEvent))
>
> growAndShrinkWidget :: MonadWidget t m => m (Dynamic t Multiple)
> growAndShrinkWidget = el "div" $ mdo
>   let initialMap = empty :: Map Int Simple
>   addButton <- button "Add"
>   countDyn <- count addButton
>   let countEvent = tagDyn countDyn addButton
>   let growEvent = fmap (\k -> singleton k (Just One)) countEvent
>   let deleteEvent = fmap (\(DeleteMe k) -> singleton k Nothing) events''
>   let updateEvents = mergeWith (union) [growEvent,deleteEvent]
>   widgets <- listWithKeyShallowDiff' initialMap updateEvents deletableWidget'
>   -- widgets :: Dynamic t (Map k (Dynamic (Simple,Event t (SimpleWidgetEvent k)))
>   let joined = joinDynThroughMap widgets -- Dynamic (Map k (Simple,Event t SimpleWidgetEvent k))
>   values <- forDyn joined (Prelude.map (fst) . elems)
>   events <- forDyn joined (Prelude.map (snd) . elems) -- Dynamic [Event (SimpleWidgetEvent)]
>   events' <- forDyn events (leftmost) -- Dynamic (Event (SimpleWidgetEvent))
>   let events0 = switchPromptlyDyn events' -- Event SimpleWidgetEvent
>   let events'' = traceEvent "delete" events0
>   hack <- holdDyn (DeleteMe 0) events''
>   display hack
>   display values
>   return values
>
> main = mainWidget $ growAndShrinkWidget >>= display
