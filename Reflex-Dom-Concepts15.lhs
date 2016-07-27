> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import qualified Data.List as List
> import Data.Functor.Misc -- For Const2
> import qualified Data.Maybe

> import           GHCJS.Types as GHCJS
> import qualified GHCJS.DOM.Event  as GHCJS (IsEvent)
> import qualified GHCJS.DOM.Element as GHCJS
> import           GHCJS.DOM.EventM as GHCJS (preventDefault, stopPropagation, EventM)

> data Simple = One | Two | Three deriving (Show)
> type Multiple = [Simple]
> data SimpleWidgetRequest = Set Simple | Flash
> data SimpleWidgetEvent k = Drop k | DragEnd k | DeleteMe k deriving (Show)

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

> builder :: (MonadWidget t m, Ord k)=> k -> Hetero -> Event t (Either SimpleWidgetRequest MiscRequest) -> m (Dynamic t (Maybe Simple,Event t (SimpleWidgetEvent k)))
> builder k (Left simp) e = do
>   let event = coincidence $ fmap (either (<$ e) (return $ (Set Three) <$ never)) e -- if e=Event Left, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   requestableSimpleWidget k simp event >>= mapDyn (\(a,b)-> (Just a, b))
> builder k (Right misc) e = do
>   let event = coincidence $ fmap (either (return $ Disable <$ never) (<$ e)) e -- if e=Event Right, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   miscWidget k event >>= mapDyn (\(a,b)-> (Nothing, b))

> miscWidget :: MonadWidget t m => k -> Event t MiscRequest -> m (Dynamic t (Simple, Event t(SimpleWidgetEvent k)))
> miscWidget key signal = do
>   (wid,deleteButton) <- elAttr' "div" attr $ do
>     liftM (DeleteMe key <$) $ button "-"
>   x <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Drop)     (void $ GHCJS.preventDefault)
>   y <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragover) (void $ GHCJS.preventDefault)
>   _ <- performEvent_ $ return () <$ y
>   let miscEvents = leftmost [(Main.Drop key <$) $ x, deleteButton]
>   return $ constDyn (One,miscEvents)
>   where
>         attr = singleton "style" "background-color: red; border: 3px solid black"

> requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t (SimpleWidgetEvent k)))
> requestableSimpleWidget key initialValue signal = mdo
>   (wid,value) <- elAttr' "div" attr $ do
>     buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>     value <-  holdDyn initialValue (leftmost buttons)
>     return value
>   x <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragend)  (void $ GHCJS.preventDefault)
>   y <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragover) (void $ GHCJS.preventDefault)
>   _ <- performEvent_ $ return () <$ y
>   let widgetEvent = (Main.DragEnd key <$) $ x
>   display value
>   forDyn value (\a -> (a,widgetEvent))
>   where
>         attr = singleton "draggable" "true"

> dragAndDropWidget :: MonadWidget t m => m (Dynamic t [Maybe Simple])
> dragAndDropWidget = el "div" $ mdo

>   let initialMap = empty :: Map Int Hetero
>   disableButton <- liftM (Right Disable <$) $ button "Disable '+' buttons"
>   let parentEvent = attachWith (\a b -> fromList (zip a (repeat b))) (current activeKeys) disableButton

>   makeSimpleWidget <- liftM (fmap (=:(Just(Left One))) . (tagDyn maxKey)) $ button "Add SimpleWidget"
>   makeMiscWidget <- liftM (fmap (=:(Just(Right Add))) . (tagDyn maxKey)) $ button "Add MiscWidget"
>   let growEvents = mergeWith makeMap [makeMiscWidget, makeSimpleWidget]
>   let updateEvents = mergeWith union [growEvents, widgetEvents', dropEvents']

>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap updateEvents parentEvent builder --MonadWidget t m => m (Dynamic t( Map k (Maybe Simple,Event t(SimpleWidgetEvent k))))
>   let widgets' = attach (current widgets) (updated widgets)
>   widgets'' <- holdDyn (Data.Map.empty,Data.Map.empty) widgets'
>   (old, new) <- splitDyn widgets''
>   (values, events) <- forDyn old (unzip . elems) >>= splitDyn
>   (values', events') <- forDyn new (unzip . elems) >>= splitDyn
>   newValues <- combineDyn zip events' values  -- Dynamic t [(Event t (SimpleWidgetEvent k), Maybe Simple)]
>   bothEvents <- combineDyn zip events events' -- Dynamic t [(Event t (SimpleWidgetEvent k), (Event t (SimpleWidgetEvent k))]

>   widgetEvents <- forDyn newValues (fmap eventBuff)
>   dropEvents <- forDyn bothEvents (fmap(fmap dropCheck))
>   let widgetEvents' = switch $ fmap (mergeWith (union)) $ current widgetEvents
>   let dropEvents' = switch $ fmap (mergeWith (union)) $ current dropEvents

newValues - forDyn into dynamic -> [(Event t (SimpleWidgetEvent k), Maybe Simple)]
          - fmap over list      -> (Event t (SimpleWidgetEvent k), Maybe Simple)
          Have to get to type (SimpleWidgetEvent k, Maybe Simple)
          - ???

Solution: zipListWithEvent

bothEvents - forDyn into Dynamic -> [(Event t (SimpleWidgetEvent k), Event t (SimpleWidgetEvent k))]
           - fmap over list      -> (Event t (SimpleWidgetEvent k), Event t (SimpleWidgetEvent k))
           - mapTuple fmap

Solution: appendEvents

(Event t (SimpleWidgetEvent Int), Event t (SimpleWidgetEvent Int)) -> Event t (Map Int (Maybe Hetero))
(Event t (SimpleWidgetEvent Int), Maybe Simple) -> Event t (Map Int (Maybe Hetero))

combineEvents :: (a -> b -> c) -> (Event a, Event b) -> Event c

>   activeKeys <- forDyn widgets keys
>   maxKey <- forDyn activeKeys (\k-> if k==[] then 0 else (maximum k)+1)
>   el "div" $ do
>     text "keys "
>     display activeKeys
>     el "div" $ return values'
>   where
>     eventBuff DeleteMe k' = k'=: Nothing
>     eventBuff Main.Drop k' =
>     dropCheck (DragEnd k, Main.Drop k') = k=: Nothing

makeMap should assign unique keys to two widgets when they're made at the same time, giving parameter 'a' the lower key

> makeMap::Map Int (Maybe Hetero) -> Map Int (Maybe Hetero) -> Map Int (Maybe Hetero)
> makeMap a b = union a $ fromList [(bKey+1,bVal)]
>   where (bKey,bVal) = elemAt 0 b

> main = mainWidget $ dragAndDropWidget>>=display
