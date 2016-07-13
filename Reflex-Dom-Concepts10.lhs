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
> type Multiple = [Simple]

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

> simpleWidget :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> simpleWidget i = el "div" $ do
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn i (leftmost buttons)
>   display value
>   return value

> addWidget :: MonadWidget t m => k -> Misc -> Event t MiscRequest -> (Dynamic t Misc)
> addWidget = do
>   a <- liftM (fmap (\_ -> Add)) $ button "Add"
>   holdDyn Empty a

> data Misc = Add deriving (Show)
> type Hetero = Either Simple Misc
> data MiscRequest = Resize | Flash
> data MiscEvent = IGotClicked

> builder :: (Ord k) => k -> Hetero -> Event t r -> m (Dynamic t (Maybe Simple))
> builder k (Left s) e = simpleWidget s >>= mapDyn (Just)
> builder k (Right m) e = liftM (Nothing <$) $ button "+" >>= holdDyn Nothing

> heterogeneousWidget :: MonadWidget t m => m ()
> heterogeneousWidget = do
>   let initialMap = [Right Add,Left One,Right Add,Left Two,Right Add,Left Three,Right Add]
>   listWithChildEvents initialMap never never builder
>   return ()
