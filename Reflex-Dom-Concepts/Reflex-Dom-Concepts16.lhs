this is a demonstration of a multi-mode widget where a value of interest is preserved while
the nature of the GUI is changed

> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2

> data Simple = One | Two | Three deriving (Show,Eq,Ord,Read)
> data SimpleWidgetRequest = Set Simple | Flash
> data WidgetEvent k = DeleteMe k | MakeSimple k deriving (Show)

> multiModeWidget :: (MonadWidget t m) => Simple -> m (Dynamic t Simple)
> multiModeWidget initialValue = do
>   mode <- el "div" $ do
>     text "Mode: "
>     modeEvents <- forM ([1,2]::[Int]) (\x -> liftM (x <$) (button (show x)))
>     mode <- holdDyn 1 $ leftmost modeEvents
>     display mode
>     return mode
>   x <- modeOneWidget initialValue
>   return x
>
> modeOneWidget :: (MonadWidget t m) => Simple -> m (Dynamic t Simple)
> modeOneWidget initialValue = do
>   a <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn initialValue $ leftmost a
>   display value
>   return value
>
> modeTwoWidget :: (MonadWidget t m) => Simple -> m (Dynamic t Simple)
> modeTwoWidget initialValue = do
>   let ddMap = constDyn $ fromList [(One,"One"),(Two,"Two"),(Three,"Three")]
>   dd <- dropdown initialValue ddMap def -- m (Dropdown k)
>   return $ _dropdown_value dd
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
