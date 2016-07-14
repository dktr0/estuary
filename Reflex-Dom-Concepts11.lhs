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
> data SimpleWidgetRequest = Set Simple | Flash

> data Misc = Add deriving (Show)
> type Hetero = Either Simple Misc
> data MiscRequest = Resize | Disable deriving (Eq)
> data MiscEvent = IGotClicked

> listWithChildEvents :: (Ord k, MonadWidget t m)
>    => Map k v                        -- an ordered map of initial values
>    -> Event t (Map k (Maybe v))      -- construction events (add/replace/delete)
>    -> Event t (Map k (Either SimpleWidgetRequest MiscRequest ))              -- events delivered to child widgets
>    -> (k -> v -> Event t (Either SimpleWidgetRequest MiscRequest) -> m a)   -- function to make a widget given key, value and request event
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

> builder :: MonadWidget t m => Int -> Hetero -> Event t (Either SimpleWidgetRequest MiscRequest) -> m (Dynamic t (Maybe Simple))
> builder k (Left s) e = do
>   let event = coincidence $ fmap (either (<$ e) (return $ (Set Three) <$ never)) e -- if e=Event Left, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   requestableSimpleWidget k s event >>= mapDyn Just
> builder k (Right m) e = do
>   let event = coincidence $ fmap (either (return $ Disable <$ never) (<$ e)) e -- if e=Event Right, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   miscWidget k event

> miscWidget:: MonadWidget t m => k -> Event t MiscRequest -> m (Dynamic t (Maybe Simple))
> miscWidget k e = do
>   let event = fmap (\a->if a==Disable then "disabled"=:"" else empty) e
>   attrs <- holdDyn empty event
>   el "div" $ do
>     buttonWithAttrs <- elDynAttr "button" attrs $ text "+"
>     return $ constDyn Nothing

> heterogeneousWidget :: MonadWidget t m => m (Dynamic t Multiple)
> heterogeneousWidget = do
>   setButton <- liftM ((fromList $ zip [0..6] $ repeat $ Left (Set Two ))<$) $ button "Set Two"
>   miscRequestButton <- liftM ((fromList $ zip [0..6 ] $ repeat $ Right (Disable))<$) $ button "Disable Add Buttons"
>   let parentEvents = leftmost [setButton,miscRequestButton]
>   let initialMap = fromList (zip [0..][Right Add,Left One,Right Add,Left Two,Right Add,Left Three,Right Add])
>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap never parentEvents builder
>   forDyn widgets (Data.Maybe.mapMaybe (id) . elems)

> main = mainWidget $ heterogeneousWidget >>= display
