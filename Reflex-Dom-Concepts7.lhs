> {-# LANGUAGE RecursiveDo #-}
> module Growing where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2
> import Multiple

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

To do next:

listWithKeyValueEvent :: (Ord k, MonadWidget t m)
   => Map k v                        -- an ordered map of initial values
   -> Event t (Map k (Maybe v))      -- add/overwrite/delete events
   -> Event t (Map k r)              -- request events
   -> (k -> v -> Event t r -> m a)   -- function to make a widget given key, value and request event
