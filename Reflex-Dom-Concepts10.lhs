> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2

> import           GHCJS.Types as GHCJS
> import qualified GHCJS.DOM.Event  as GHCJS (IsEvent)
> import qualified GHCJS.DOM.Element as GHCJS
> import           GHCJS.DOM.EventM as GHCJS (preventDefault, stopPropagation, EventM)

> data Simple = One | Two | Three deriving (Show)
> data Misc = Add | Drop | Empty deriving (Show)
> type Multiple = [Simple]
> type Hetero = [Either Simple Misc]

> data MiscRequest = Resize | Flash

> listWithChildEvents' :: (Ord k, MonadWidget t m)
>    => Map k v                        -- an ordered map of initial values
>    -> Event t (Map k (Maybe v))      -- construction events (add/replace/delete)
>    -> Event t (Map k r)              -- events delivered to child widgets
>    -> [(k -> v -> Event t r -> m a)]   -- function to make a widget given key, value and request event
>    -> m (Dynamic t (Map k Either a b))
>
> listWithChildEvents' initial cEvents rEvents mkChild = do
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


> dropWidget :: MonadWidget t m => k -> Misc -> Event t MiscRequest -> (Dynamic t Misc)
> dropWidget = do
>   (dropArea, _) <- el' "div"
>   x <- R.wrapDomEvent (R._el_element dropArea) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
>   y <- R.wrapDomEvent (R._el_element dropArea) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
>   z <- R.wrapDomEvent (R._el_element dropArea) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
>   _ <- R.performEvent_ $ return () <$ y
>   a <- liftM (fmap (\_ -> Drop)) $ x
>   holdDyn Empty a

> addWidget :: MonadWidget t m => k -> Misc -> Event t MiscRequest -> (Dynamic t Misc)
> addWidget = do
>   a <- liftM (fmap (\_ -> Add)) $ button "Add"
>   holdDyn Empty a

> heterogeneousWidget :: MonadWidget t m => (Dynamic t Hetero)
> heterogeneousWidget = do
>   widgetList = [requestableSimpleWidget, addWidget, requestableSimpleWidget, addWidget, requestableSimpleWidget]
>   partial <- listWithChildEvents initialMap never never
>   listMap <- liftM (joinDynThroughMap) $ map partial widgetList
>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap never never widgetList
>   values <- forDyn widgets (elems)
>   display values
