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

> data Simple = One | Two | Three deriving (Show)
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

> simpleWidget :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> simpleWidget i = el "div" $ do
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn i (leftmost buttons)
>   display value
>   return value

> data Misc = Add deriving (Show)
> type Hetero = Either Simple Misc
> data MiscRequest = Resize | Flash
> data MiscEvent = IGotClicked

? builder' :: (MonadWidget t m, Ord k) -> k -> Either a b -> Event t (Either c d) -> m (Dynamic t (Maybe e))

metabuilder :: (k -> v -> Event t r -> m v) -> (k -> w -> Event t s -> m b) -> (k -> Either v w -> Event t (Either r s))
  -> m (Dynamic t (Maybe v))

> builder :: MonadWidget t m => Int -> Hetero -> Event t r -> m (Dynamic t (Maybe Simple))
> builder k (Left s) e = simpleWidget s >>= mapDyn (Just)
> builder k (Right m) e = do
>  addbutton <- liftM (Nothing <$) $ button "+"
>  holdDyn Nothing addbutton

> heterogeneousWidget :: MonadWidget t m => m (Dynamic t Multiple)
> heterogeneousWidget = do
>   let initialMap = fromList (zip [0..][Right Add,Left One,Right Add,Left Two,Right Add,Left Three,Right Add])
>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap never never builder
>   forDyn widgets (Data.Maybe.mapMaybe (id) . elems)

> main = mainWidget $ heterogeneousWidget >>= display
