> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2

> data Simple = One | Two | Three deriving (Show)
> type Multiple = [Simple]

Next Steps:

1. widgets that can send signals to their parents:

> data SimpleWidgetEvent k = DeleteMe k deriving (Show)
>
> deletableWidget :: (Ord k, MonadWidget t m) =>
>   k -> Simple -> Event t Simple -> m (Dynamic t Simple, Event t (SimpleWidgetEvent k))
> deletableWidget k i _ = el "div" $ do
>   deleteEvent <- liftM (DeleteMe k <$) $ button "-"
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn i (leftmost buttons)
>   return (value,deleteEvent)

This (above) would probably work but the return type doesn't match what we need for listWithKeyShallowDiff... so
let's make a return type that is a tuple inside a Dynamic instead:

> deletableWidget' :: (Ord k, MonadWidget t m) =>
>   k -> Simple -> Event t Simple -> m (Dynamic t (Simple, Event t (SimpleWidgetEvent k)))
> deletableWidget' k i _ = el "div" $ do
>   deleteEvent <- liftM (DeleteMe k <$) $ button "-"
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn i (leftmost buttons)
>   display value
>   forDyn value (\a -> (a,deleteEvent))
>
> countEvent :: (MonadWidget t m, Num k, Enum k) => Event t a -> m (Event t k)
> countEvent = zipListWithEvent (\a _ -> a) [0..]
>
> growAndShrinkWidget :: MonadWidget t m => m (Dynamic t Multiple)
> growAndShrinkWidget = el "div" $ mdo
>   let initialMap = empty :: Map Int Simple
>   growEvent <- (return . (fmap (\k -> singleton k (Just One)))) =<< countEvent =<< button "-"
>   let updateEvents = mergeWith (union) ([growEvent]++[deleteEvents'''])
>   widgets <- liftM (joinDynThroughMap) $ listWithKeyShallowDiff initialMap updateEvents deletableWidget'
>   values <- forDyn widgets (Prelude.map (fst) . elems)
>   events <- forDyn widgets (Prelude.map (snd) . elems) -- Dynamic [Event (SimpleWidgetEvent)]
>   deleteEvents <- forDyn events (fmap (fmap (\(DeleteMe k)-> singleton k Nothing)))
>   let deleteEvents' = switch $ fmap (mergeWith (union)) $ current deleteEvents -- Behaviour [Event Map ...]
>   display values
>   return values
>
> main = mainWidget $ growAndShrinkWidget >>= display
