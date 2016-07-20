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
> data Block = One | Two | Three deriving (Show)
> type Multiple = [Simple]
> data SimpleBlockRequest = Set Block | Flash
> data SimpleBlockEvent k = Drop k | DragEnd k | DeleteMe k deriving (Show)

> data Misc = Add deriving (Show)
> type Hetero = Either Simple Misc
> data MiscRequest = Resize | Disable deriving (Eq)
> data MiscEvent = IGotClicked

Builds the list of simpleWidgets

> builder :: (MonadWidget t m, Ord k) => k -> Hetero -> Event t (Either SimpleBlockRequest MiscRequest) -> m (Dynamic t (Maybe Simple,Event t (SimpleBlockEvent k)))
> builder key (Left s) signal = do
>   let event = coincidence $ fmap (either (<$ signal) (return Set Sample) <$ never)) signal
>   simpleBlock key s event >>= mapDyn (\(a,b) -> (Just a, b))
> builder key (Right m) signal = do
>   let event = coincidence $ fmap (either (<$ signal) (return $ Disable <$ never)) signal
>   miscWidget key event >>= mapDyn (\(a,b) -> (Nothing, b))

Add button that is created when a new simpleBlock is created, return simple type and child events.

Pass in disable Event
> miscWidget :: MonadWidget t m => k -> Event t MiscRequest -> m (Dynamic t (Block, Event t(SimpleBlockEvent k)))
> miscWidget key signal = do
>   el "div" $ do
>     let event = fmap (\a->if a==Disable then "disabled"=:"" else empty) signal
>     attrs <- holdDyn empty event
>     buttonWithAttrs <- elDynAttr "button" attrs $ text "    +    "
>     deleteButton <- liftM (DeleteMe key <$) $ button "-"
>     miscEvents <- leftmost [addButton, deleteButton]
>     return $ constDyn (Modifier,miscEvents)

simpleBlock that displays a name, returns its type and any dragend and drop events.

Have to fix dropdown menu so that it returns values of type Block. (String != Block)

> simpleBlock :: MonadWidget t m => k -> Simple -> Event t (SimpleBlockRequest) -> m (Dynamic t (Block, Event t (SimpleBlockEvent k)))
> simpleBlock key initialValue signal = do
>   elDynAttr "div" attr $ do

Create dropdown menu
>     d <- dropdown initialSample (constDyn samples) def
>     let dropDownEvent = tagDyn (_dropdown_value d) (_dropdown_change d)

Track flash event
>     let flashEvent = fforMaybe signal f
>     flashToggle <- toggle True flashEvent
>     attr <- forDyn flashToggle h

Track set event
>     let setEvent = fforMaybe signal s

Display value
>     value <- holdDyn initialValue leftmost [dropDownEvent, setEvent]
>     display value

Mouse event listeners
>     x <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
>     y <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
>     z <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
>     _ <- R.performEvent_ $ return () <$ y
>     mouseEvent <- leftmost [ x <$ Drop key , z <$ DragEnd key]

>     forDyn value (\a -> (Sample,mouseEvent))

>   where s (Set x) = Just x
>         s _       = Nothing
>         f (Flash) = Just ()
>         f _       = Nothing
>         h True    = singleton "draggable" "true" "style" "background-color: red; border: 3px solid black"
>         h False   = singleton "draggable" "true" "style" "background-color: green; border: 3px solid black"
>         samples   = fromList [("sn","sn"),("bd","bd"),("arpy","arpy"),("arp","arp"),("hh","hh"),("ht","ht")]

> dragAndDropWidget :: MonadWidget t m => m (Dynamic t [Maybe Simple])
> dragAndDropWidget = el "div" $ mdo
simpleWidgetButton <- button "Add SimpleWidget"
miscWidgetButton <- button "Add miscButton"

let simpleWidgetButton' = tagDyn maxKey simpleWidgetButton
let simpleWidgetButton'' = fmap (\k-> singleton k (Just (Left One))) simpleWidgetButton' -- ::MonadWidget t m => m (Event t (Map Int (Maybe Hetero)))

let miscWidgetButton' = tagDyn maxKey miscWidgetButton
let miscWidgetButton'' = fmap (\k-> singleton k (Just (Right Add))) miscWidgetButton' -- ::MonadWidget t m => m (Event t (Map Int (Maybe Hetero)))

let growEvents = mergeWith makeMap [miscWidgetButton'', simpleWidgetButton'']
>   let initialMap = empty :: Map Int Hetero
let updateEvent = mergeWith union [growEvents, deleteEvents']

setButton <- liftM (Left (Set Two) <$) $ button "Set two"
let setTwoEvent = attachWith (\a b -> fromList (zip a (repeat b))) (current activeKeys) setButton -- Event t (Map (activeKeys) (Maybe (Either Simp/MiscReq)))
> disableButton <- liftM (Right Disable <$) $ button "Disable '+' buttons"
> let disableEvent = attachWith (\a b -> fromList (zip a (repeat b))) (current activeKeys) disableButton

> -- @use something more meaningful than 'leftmost'
>   let parentEvents = leftmost [disableEvent,setTwoEvent]

>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap updateEvent parentEvents builder --MonadWidget t m => m (Dynamic t( Map k (Maybe Simple,Event t(SimpleWidgetEvent k))))
>   let widgets' = attach (current widgets) (updated widgets)
>   values <- forDyn widgets $ fst (Prelude.map fst . elems)
>   events <- forDyn widgets $ fst (Prelude.map snd . elems) -- Dynamic t [Event ..]
>   values' <- forDyn widgets $ snd (Prelude.map fst . elems)
>   events' <- forDyn widgets $ snd (Prelude.map snd . elems)

Cant insert into map, have to write function
>   addEvents <- forDyn events' (fmap (fmap (\Add k)))
>
>   dropEvents <- combineDyn (fmap (fmap (\(DragEnd k)(Drop k') -> )))events events'
>   dropEvents' <- switch $ fmap (mergeWith (union)) $ current dropEvents
>   deleteEvents <- forDyn events' (fmap (fmap (\(DeleteMe k) -> singleton k Nothing))) -- m (Dynamic t [Event t (Map k Nothing...)])
>   let deleteEvents' = switch $ fmap (mergeWith (union)) $ current deleteEvents -- Behaviour [Event Map ...]

>   activeKeys <- forDyn widgets (keys)
>   maxKey <- forDyn activeKeys (\k-> if k==[] then 0 else (maximum k)+1)
>   el "div" $ do
>     text "keys "
>     display activeKeys
>     el "div" $ return values
