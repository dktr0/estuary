In this example, we navigate between multiple widgets, where each widget is able
to make changes to a shared global state (in this case, a Simple value in an MVar).

> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Control.Concurrent

> data Simple = One | Two | Three deriving (Show,Eq,Ord,Read)

> main = do
>   sharedState <- newMVar One
>   let pageMenu = fromList [(1,"page 1"),(2,"page 2")]
>   let pageMap = fromList [(1,pageOne),(2,pageTwo)]
>   pageDropDown <- dropdown 1 pageMenu def
>   let widgetChange = fmap (\x -> (lookup x pageMap) sharedState) $ _dropdown_value pageDropDown
>   el "div" $ widgetHold (pageOne sharedState) widgetChange

> pageOne :: MonadWidget t m => MVar Simple -> m ()
> pageOne mSimple = readMVar mSimple >>= \simple -> do
>   events <- liftM (leftmost) $ forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   holdDyn simple events >>= display
>   performEvent_ $ fmap (liftIO . swapMVar mSimple) $ events
>   return ()

> pageTwo :: MonadWidget t m => MVar Simple -> m ()
> pageTwo mSimple = readMVar mSimple >>= \simple -> do
>   let menu = fromList [(One,"One"),(Two,"Two"),(Three,"Three")]
>   dd <- dropdown simple menu def
>   let events = _dropdown_value dd
>   holdDyn simple events >>= display
>   performEvent_ $ fmap (liftIO . swapMVar mSimple) $ events
>   return ()
