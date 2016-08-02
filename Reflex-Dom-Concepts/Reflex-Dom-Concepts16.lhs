this is a demonstration of a multi-mode widget where a value of interest is preserved while
the nature of the GUI is changed

> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map

Note: we need to make sure that Simple is an instance of Ord and Read
in order to be able to use Simple values directly as keys in the map
used to set up drop-downs in Reflex. We can derive them automatically.

> data Simple = One | Two | Three deriving (Show,Eq,Ord,Read)
> data SimpleWidgetRequest = Set Simple | Flash
> data WidgetEvent k = DeleteMe k | MakeSimple k deriving (Show)

We'll start by making a simplified example of widget that can present/interact
around a Simple value in one of two ways. Then we'll make a second version
that incorporates the key maintenance and event channels we've developed
in other recent examples.

We'll define two parallel widgets for editing a simple value, one based on
buttons, the other based on a drop-down menu. Then we'll make a multiModeWidget
that can switch between these two widgets.

> modeWidget :: (MonadWidget t m) => Int -> Simple -> m (Dynamic t Simple)
>
> modeWidget 1 initialValue = do
>   a <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn initialValue $ leftmost a
>   display value
>   return value
>
> modeWidget 2 initialValue = do
>   let ddMap = constDyn $ fromList [(One,"One"),(Two,"Two"),(Three,"Three")]
>   dd <- dropdown initialValue ddMap def -- m (Dropdown k)
>   return $ _dropdown_value dd
>
> multiModeWidget :: (MonadWidget t m) => Simple -> m (Dynamic t Simple)
> multiModeWidget initialValue = mdo
>   mode <- el "div" $ do
>     text "Mode: "
>     modeEvents <- forM ([1,2]::[Int]) (\x -> liftM (x <$) (button (show x)))
>     mode <- holdDyn 1 $ leftmost modeEvents
>     display mode
>     return mode
>   let modeAndValue = attachPromptlyDyn (updated value) mode -- :: Event (Simple,Int)
>   let changeWidget = forM modeAndValue (\(a,b) -> modeWidget b a) -- Event t (m (Dynamic t Simple))
>   wh <- widgetHold (modeWidget 1 initialValue) changeWidget -- m (Dynamic t (Dynamic t Simple))
>   let value = join wh
>   return value
>
> main = mainWidget $ multiModeWidget One >>= display



multiModeWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t (WidgetEvent k)))

requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t (WidgetEvent k)))
requestableSimpleWidget key initialValue signal = do
  let flashEvent = fforMaybe signal g
  flashToggle <- toggle True flashEvent
  attr <- forDyn flashToggle h
  let buttons = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
  buttons' <- elDynAttr "div" attr buttons
  deleteButton <- liftM (DeleteMe key <$) $ button "-"
  let setEvent = fforMaybe signal f
  value <- holdDyn initialValue (leftmost (buttons'++[setEvent]))
  display value
  forDyn value (\a-> (a,deleteButton))
  where f (Set x) = Just x
        f _ = Nothing
        g (Flash) = Just ()
        g _ = Nothing
        h True = singleton "style" "background-color: red; border: 3px solid black"
        h False = singleton "style" "background-color: green; border: 3px solid black"
