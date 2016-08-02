This is a demonstration of a multi-mode widget where a value of interest is preserved while
the nature of the GUI is changed. We'll just make a simple example here - with none of the additionally key
maintenance and child-parent or parent-child signalling found in previous examples.

> {-# LANGUAGE RecursiveDo #-}
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
> multiModeWidget :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> multiModeWidget initialValue = mdo
>   text "Mode: "
>   m1 <- liftM (1 <$) $ button "mode1"
>   m2 <- liftM (2 <$) $ button "mode2"
>   let valueAndMode = attach (current value) $ leftmost [m1,m2]
>   let widgetChange = fmap (\(v,mode) -> modeWidget mode v) valueAndMode
>   wh <- widgetHold (modeWidget 1 initialValue) widgetChange
>   let value = joinDyn wh
>   return value
>
> main = mainWidget $ multiModeWidget One >>= display
