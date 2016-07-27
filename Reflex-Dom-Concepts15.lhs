s> {-# LANGUAGE RecursiveDo #-}
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
> data Block = One | Two | Three deriving (Show)
> type Multiple = [Simple]
> data SimpleBlockRequest = Set Block | Flash
> data SimpleBlockEvent k = Drop k | DragEnd k | DeleteMe k deriving (Show)

> data Misc = Add deriving (Show)
> type Hetero = Either Simple Misc
> data MiscRequest = Resize | Disable deriving (Eq)
> data MiscEvent = IGotClicked

> listWithChildEvents :: (Ord k, MonadWidget t m)
>    => Map k v                        -- an ordered map of initial values
>    -> Event t (Map k (Maybe v))      -- construction events (add/replace/delete)
>    -> Event t (Map k (Either SimpleWidgetRequest MiscRequest ))             -- events delivered to child widgets
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

Builds the list of simpleWidgets

> builder :: (MonadWidget t m, Ord k) => k -> Hetero -> Event t (These SimpleWidgetRequest MiscRequest) -> m (Dynamic t (Maybe Simple,Event t (SimpleWidgetEvent k)))
> builder key (Left s) signal = do
>   let event = coincidence $ fmap (either (<$ signal) (return Set Sample) <$ never)) signal
>   simpleBlock key s event >>= mapDyn (\(a,b) -> (Just a, b))
> builder key (Right m) signal = do
>   let event = coincidence $ fmap (either (<$ signal) (return $ Disable <$ never)) signal
>   miscWidget key event >>= mapDyn (\(a,b) -> (Nothing, b))


Pass in disable Event
> miscWidget :: MonadWidget t m => k -> Event t MiscRequest -> m (Dynamic t (Simple, Event t(SimpleWidgetEvent k)))
> miscWidget key signal = do
>   el "div" $ do
>     let event = fmap (\a->if a==Disable then "disabled"=:"" else empty) signal
>     attrs <- holdDyn empty event
>     buttonWithAttrs <- elDynAttr "button" attrs $ text "    +    "
>     deleteButton <- liftM (DeleteMe key <$) $ button "-"
>     miscEvents <- leftmost [addButton, deleteButton]
>     return $ constDyn (Modifier,miscEvents)

> requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t (SimpleWidgetEvent k)))
> requestableSimpleWidget key initialValue signal = do
>   let buttons = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   buttons' <- elDynAttr "div" attr buttons
>   deleteButton <- liftM (DeleteMe key <$) $ button "-"

Mouse event listeners
>     x <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
>     y <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
>     z <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
>     _ <- R.performEvent_ $ return () <$ y
>     widgetEvent <- leftmost [ x <$ Drop key , z <$ DragEnd key, deleteButton]

>   value <- holdDyn initialValue widgetEvent
>   display value
>   forDyn value (\a -> (a,widgetEvent))

> dragAndDropWidget :: MonadWidget t m => m (Dynamic t [Maybe Simple])
> dragAndDropWidget = el "div" $ mdo

>   let initialMap = empty :: Map Int Hetero
>   disableButton <- liftM (Right Disable <$) $ button "Disable '+' buttons"
>   let parentEvent = attachWith (\a b -> fromList (zip a (repeat b))) (current activeKeys) disableButton

>   makeSimpleWidget <- liftM (fmap (=:(Just(Left One))) . (tagDyn maxKey)) $ button "Add SimpleWidget"
>   makeMiscWidget <- liftM (fmap (=:(Just(Right Add))) . (tagDyn maxKey)) $ button "Add MiscWidget"
>   let growEvents = mergeWith makeMap [makeMiscWidget, makeSimpleWidget]
>   let updateEvent = mergeWith union [growEvents, widgetEvents']

>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap updateEvents' parentEvent builder --MonadWidget t m => m (Dynamic t( Map k (Maybe Simple,Event t(SimpleWidgetEvent k))))
>   let widgets' = attach (current widgets) (updated widgets)
>   (values, events) <- forDyn widgets' (fst (unzip . elems)) >>= splitDyn
>   (values', events') <- forDyn widgets' (snd (unzip . elems)) >>= splitDyn
>   newValue <- combineDyn (\a b -> (a,b)) events' values  --(newEvent, oldValue)

>   widgetEvents <- forDyn newValue (fmap (fmap eventBuff))
>   let updateEvents' = switch $ fmap (mergeWith (union)) $ current widgetEvents

have dropEvent trigger delete event on old key value than merge these events and pass as updateEvents'

>   activeKeys <- forDyn widgets keys
>   activeKeys <- forDyn widgets (keys)
>   maxKey <- forDyn activeKeys (\k-> if k==[] then 0 else (maximum k)+1)
>   el "div" $ do
>     text "keys "
>     display activeKeys
>     el "div" $ return values
>   where
>     eventBuff (DeleteMe k', v) = k':= Nothing
>     eventBuff (Drop k', v) = k':= Just (v)
