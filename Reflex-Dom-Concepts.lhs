> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map

In these examples, we'll build Reflex.Dom widgets around a simple pure data
type, and (soon) more complex types built up out of that simple pure type.
Here's the type, which has exactly three possible values (data constructors).
We are automatically deriving definitions for the Show type class, which will
undoubtedly be useful at various points.

> data Simple = One | Two | Three deriving (Show)

Our widgets will have a return type of m (Dynamic t a). In this first set of
examples, we'll focus on widgets that are meant to manipulate just the simple
type, so their return type will be m (Dynamic t Simple).

As a first attempt, we'll make a widget that has three buttons and whichever
button is pressed most recently defines the value returned by the widget as a
whole.

> simpleWidget' :: MonadWidget t m => m (Dynamic t Simple)
> simpleWidget' = do
>   a <- button "One" -- :: m (Event t ())
>   b <- button "Two"
>   c <- button "Three"
>   let d = fmap (\() -> One) a -- :: Event t Simple
>   let e = fmap (\() -> Two) b
>   let f = fmap (\() -> Three) c
>   holdDyn One $ leftmost [d,e,f] -- m (Dynamic t Simple)
>
> main0 = mainWidget $ simpleWidget' >>= display

In the code above, we have buttons that produce Events containing the
an empty (), signalling that something has happened but nothing beyond that.
We map those into Events that contain values corresponding to the meaning
of which button was pressed. We use leftmost to turn those three streams of
Events into a single Event stream. Finally holdDyn lets us turn this into
a Dynamic value, with a default value (One) chosen from our type as the initial
value of this Dynamic.

In the IO () program defined by main, we bind simpleWidget to 'display', taking
advantage of the fact that our Simple type is a member of the class Show. (To verify
that the examples below all work you'll need to change the definition of main to
point to successive variations simpleWidget' simpleWidget'' etc.)

It's a bit tedious the way we have three lines of code to make the buttons and
then another three lines of code just, effectively, to make the buttons trigger
a different, specific event (One, Two or Three instead of ()). This next example
shows three similar and more economical ways of mapping what is returned by the
buttons:

> simpleWidget'' :: MonadWidget t m => m (Dynamic t Simple)
> simpleWidget'' = do
>   a <- liftM (fmap (\_ -> One)) $ button "One" -- :: m (Event t Simple)
>   b <- liftM (fmap (const Two)) $ button "Two"
>   c <- liftM (Three <$) $ button "Three"
>   holdDyn One $ leftmost [a,b,c] -- m (Dynamic t Simple)

Now, for an exercise, let's eliminate the redundancy of three lines for three
buttons, when we can just make a list of the elements to be "buttonized", taking
advantage of the fact that our simple type derives Show to "automatically" generate
labels for the buttons.

> simpleWidget''' :: MonadWidget t m => m (Dynamic t Simple)
> simpleWidget''' = do
>   a <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   holdDyn One $ leftmost a

It will often be useful during development to include extra visual feedback within
a widget's definition. In the following example that will produce the same text
displayed twice, but in the coming examples involving composite widgets that will
no longer be the case.

> simpleWidget'''' :: MonadWidget t m => m (Dynamic t Simple)
> simpleWidget'''' = do
>   a <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn One $ leftmost a
>   display value
>   return value

In a final, definitive variation on our basic widget for our Simple type, we add
the ability to specify the initial Simple value of the widget, and wrap everything
in a <div> element so that each widget will appear on a new "line" in the browser.
(In a  further variation we could add CSS styles to both the buttons and the
containing div.)

> simpleWidget :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> simpleWidget i = el "div" $ do
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn i (leftmost buttons)
>   display value
>   return value

Now we experiment with building widgets for more complex types built on top of
the Simple type, and using the simpleWidget definition. For example, a tuplet
composed of a pair of Simple values and edited with a pair of simpleWidgets.

> type Tuplet = (Simple,Simple)
>
> tupletWidget :: MonadWidget t m => Tuplet -> m (Dynamic t Tuplet)
> tupletWidget (i,j) = el "div" $ do
>   i' <- simpleWidget i
>   j' <- simpleWidget j
>   combineDyn (\x y -> "composite:" ++ show x ++ " " ++ (show y)) i' j' >>= display
>   combineDyn (,) i' j'
>
> main' = mainWidget $ tupletWidget (One,One) >>= display

What about a list containing a definite number of Simple types?

> type Multiple = [Simple]
>
> listWidget :: MonadWidget t m => Multiple -> m (Dynamic t Multiple)
> listWidget i = el "div" $ do
>   children <- forM i simpleWidget
>   value <- foldM (combineDyn (\ys y -> ys ++ [y])) (constDyn []) children
>   display value
>   return value
>
> main'' = mainWidget $ listWidget [One,Two,Three,Two,One] >>= display

So far so good, but what we really want to be able to do is have a variable
number of "children" in a container widget like this. Let's start by making
a widget with a button to grow the value by adding extra Simple values and
widgets.

> growingWidget :: MonadWidget t m => m (Dynamic t Multiple)
> growingWidget = do
>   growButton <- liftM ((++[One]) <$) $ button "Add"
>   values <- foldDyn ($) [] growButton -- m (Dynamic t Multiple)
>   let builder xs = forM xs simpleWidget >>= sequenceDyn   --  m [Dynamic t Simple] >>= m (Dynamic t Multiple)
>   widgets <- widgetHold (builder []) $ fmap (builder) (updated values) -- m (Dynamic (Dynamic t Multiple))
>   return $ joinDyn widgets
>
> sequenceDyn :: MonadWidget t m => [Dynamic t a] -> m (Dynamic t [a])
> sequenceDyn = foldM (combineDyn (\ys y -> ys ++ [y])) (constDyn [])
>
> main''' = mainWidget $ growingWidget' >>= display

There is a problem with the code above, which may not be obvious until you go
to test it (it's not flagged at compile time). 'values' is generated by
folding in changes triggered by the Add button. This means that any changes to
the values that come from within each simpleWidget will be discarded/forgotten
any time the Add button is pressed (although they will appear in the Dynamic
result of the function until that time). Ok, let's take another stab at it,
folding in any updates that come out of the widgets as well:

> growingWidget' :: MonadWidget t m => m (Dynamic t Multiple)
> growingWidget' = mdo
>   growButton <- liftM ((++[One]) <$) $ button "Add"
>   values <- foldDyn ($) [] $ leftmost [growButton,updates]
>   let builder xs = forM xs simpleWidget >>= sequenceDyn
>   widgets <- widgetHold (builder []) $ fmap (builder) (updated values)
>   let widgets' = joinDyn widgets
>   let updates = fmap (const) $ updated widgets'    -- m (Event t (Multiple -> Multiple))
>   display values
>   return widgets'

The code above compiles but doesn't work at runtime - when you press the Add
button things get "unstable", flickering back and forth between different
states and being generally unresponsive. We think it is because we are making
an infinite cycle of pushing update events that came from the widgets back in
to the widgets. Hmmm... back to the drawing board. This function looks promising:

listWithKeyShallowDiff :: (Ord k, MonadWidget t m) =>
  Map k v  ->
  Event t (Map k (Maybe v)) ->
  (k -> v -> Event t v -> m a) ->
  m (Dynamic t (Map k a))

> growingWidget'' :: MonadWidget t m => m (Dynamic t Multiple)
> growingWidget'' = el "div" $ do
>   let initialMap = empty :: Map Int Simple
>   addButton <- button "Add"
>   countDyn <- count addButton -- :: MonadWidget t m => m (Dynamic t Int)
>   let countEvent = tagDyn countDyn addButton
>   let growEvent = fmap (\k -> singleton k (Just One)) countEvent
>   let builder key iValue event = simpleWidget iValue
>   widgets <- listWithKeyShallowDiff initialMap growEvent (builder)
>   -- so widgets :: Dynamic t (Map k (Dynamic t Simple))
>   let values = joinDynThroughMap widgets -- Dynamic (Map k Simple)
>   values' <- forDyn values elems -- m (Dynamic t [Simple])
>   display values'
>   return values'
>
> main'''' = mainWidget $ growingWidget'' >>= display

That's working nicely now!

(Below is failed attempt to use dyn instead of widgetHold...

> simpleBuilder :: MonadWidget t m => Multiple -> m (Dynamic t Multiple)
> simpleBuilder xs = forM xs simpleWidget >>= sequenceDyn
>
> dynArg :: MonadWidget t m => Dynamic t Multiple -> m (Dynamic t (m (Dynamic t Multiple)))
> dynArg x = forDyn x simpleBuilder
>
> growingWidget''' :: MonadWidget t m => m (Dynamic t Multiple)
> growingWidget''' = el "div" $ mdo
>   addButton <- liftM ((++[One]) <$) $ button "Add"
>   let updates = fmap (const) widgets''
>   values <- foldDyn ($) [] $ leftmost [addButton,updates]
>   widgets0 <- dynArg values
>   widgets <- dyn widgets0 -- m (Event t (Dynamic t Multiple))
>   let widgets' = fmap (updated) widgets -- (Event t (Event t Multiple))
>   let widgets'' = coincidence widgets' -- (Event t Multiple)
>   widgets''' <- holdDyn [] widgets''
>   display widgets'''
>   return widgets'''

So much for that...

Next steps:

1. widgets that can send signals to their parents:

> data SimpleWidgetEvent k = DeleteMe k deriving (Show)
>
> deletableWidget :: (Ord k, MonadWidget t m) =>
>   k -> Simple -> Event t Simple -> m (Dynamic t Simple, Event t (SimpleWidgetEvent k))
> deletableWidget k i _ = el "div" $ do
>   deleteEvent <- liftM (DeleteMe k <$) $ button "-"
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn i (leftmost buttons)
>   display value
>   return (value,deleteEvent)
>
> deletableWidget' :: MonadWidget t m =>
>   Int -> Simple -> Event t Simple -> m (Dynamic t (Simple, Event t (SimpleWidgetEvent Int)))
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
>   widgets <- listWithKeyShallowDiff initialMap updateEvents deletableWidget'
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


oinDynThroughMap :: Ord k => Dynamic (Map k (Dynamic a)) ->  Dynamic (Map k a)

2. then, widgets that can receive signals from their parents (i.e. "flash")

> data SimpleWidgetRequest = Set Simple | Flash

widget :: (Ord k, MonadWidget t m) =>
  k -> SimpleWidgetRequest -> Event t SimpleWidgetRequest
    -> m (Dynamic t Simple, Event t SimpleWidgetEvent)
widget k (Set i) e = el "div" $ do ...

k -> v -> Event v -> m a

k -> Simple -> Event Simple -> m Dynamic Simple

k -> v -> Event u -> m a

k -> Either Simple SimpleWidgetRequest -> Event (Either Simple SimpleWidgetRequest) -> m Dynamic Simple

elDynAttr  :: String -> Dynamic (Map String String) -> m a -> m a

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
>
> main = mainWidget $ do
>  flash <- liftM (Flash <$) $ button "flash"
>  makeThree <- liftM (Set Three <$) $ button "makeThree"
>  let requests = leftmost [flash,makeThree]
>  widget1 <- requestableSimpleWidget 1 One requests
>  widget2 <- requestableSimpleWidget 2 Two requests
>  return ()


listWithKeyShallowDiff :: (Ord k, MonadWidget t m) =>
  Map k v  ->
  Event t (Map k (Maybe v)) ->
  (k -> v -> Event t v -> m a) ->
  m (Dynamic t (Map k a))


We anticipate an issue to puzzle about in the above is the fact that the initializer
and the update type in the builder function expected by listWithKeyShallowDiff
are the same. We can include a setter/initializer in an algebraic type constructed
to pass events in to the widget but what happens when the initial value is not
a setter/initializer? Will this error be flagged at compile time like we want?
I don't think so because the initializer is provided (for example) when the add
button is pressed in our examples above - this is runtime. We may need to create
a hacked version of listWithKeyShallowDiff that has different types for initializer
and update events.
