> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Either
> import Safe (readMay)

> data Simple = One | Two | Three deriving (Show)
> data SimpleWidgetRequest = Set Simple | Flash | MakeBlue deriving (Show)
> data SimpleWidgetEvent k = DeleteMe k |FlashIndividual k deriving (Show)
> type Multiple = [Simple]

> notifiableWidget':: (Ord k, MonadWidget t m ) => k -> Either Simple SimpleWidgetRequest-> Event t (Either Simple SimpleWidgetRequest) -> m (Dynamic t (Simple,Event t (SimpleWidgetEvent k)))
> notifiableWidget' k initialVal parentNotif = el "div" $ do
>   let flashEvent = fforMaybe parentNotif g
>   flashToggle <- toggle True flashEvent
>   attr <- forDyn flashToggle setAttrs
>   let buttons' = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   buttons <- elDynAttr "div" attr buttons'
>   deleteEvent <- liftM (DeleteMe k <$) $ button "-"
>   flashIndividualEvent <- liftM (FlashIndividual k <$) $ button "Flash Individual"
>   let events = leftmost [deleteEvent, flashIndividualEvent]
>   let initialVal' = either (\x->x) (\_ -> One) initialVal
>   let event = fforMaybe parentNotif f
>   value <- holdDyn (initialVal') $ leftmost (buttons++[event])
>   display value
>   forDyn value (\a -> (a,events))
>   where f (Right (Set x)) = Just x
>         f (Right Flash) = Nothing
>         f (Left x) = Just x -- Same action as a 'set'
>         f _ = Nothing
>         g (Right Flash) = Just ()
>         g _ = Nothing
>         setAttrs tog = if tog then ("style"=:"background-color: red;") else ("style"=:"background-color: blue;")


> growAndShrinkWidget' :: MonadWidget t m => m (Dynamic t Multiple)
> growAndShrinkWidget' = el "div" $ mdo
>   let initialMap = empty :: Map Int (Either Simple SimpleWidgetRequest)

>   addButton <- button "Add"
>   makeThreeButton <- liftM (Just (Left Three)<$)$ button "Make all three"
>   setTwoButton <- liftM (Just (Right (Set Two))<$) $ button "SET all two"
>   flashButton <- liftM (Just (Right Flash) <$) $ button "Flash"--  :: MonadWidget t m => m (Event t (Just $ Either Simple SimpleWidgetRequest))
>   targetFlashButton <- liftM (Just (Right Flash) <$) $ button "Target flash"--  :: MonadWidget t m => m (Event t (Just $ Either Simple SimpleWidgetRequest))
>   flashTargetField <- textInput $ def & textInputConfig_inputType .~"number"
>   let flashTarget = _textInput_value flashTargetField
>   flashTarget' <- mapDyn  (\k-> if (readMay k::Maybe Int) == Nothing then [] else [read k::Int] ) flashTarget
>   countDyn <- count addButton
>   flashTargetEvent <- makeDynMap flashTarget' targetFlashButton
>   let countEvent = tagDyn countDyn addButton
>   let growEvent = fmap (\k -> singleton k (Just $ Left One)) countEvent -- Event t (Map k (Maybe (Either Simple SimpleWidgetRequest)))  -- create new notifiableWidget with value defaulted to One
>   flashEvent <- makeDynMap activeKeys flashButton
>   makeThreeEvent <- makeDynMap activeKeys makeThreeButton
>   setTwoEvent <- makeDynMap activeKeys setTwoButton
>   let childEvent = fmap determineChildEvent events''
>   let updateEvents = mergeWith (union) $ [growEvent,childEvent,flashEvent, makeThreeEvent, setTwoEvent, flashTargetEvent]

>   widgets <- listWithKeyShallowDiff initialMap updateEvents notifiableWidget'
>   let joined = joinDynThroughMap widgets -- Dynamic t( Map k (Simple, Event t (SimpleWidgetEvent k)))
>   activeKeys <- forDyn widgets keys  -- ::(MonadWidget t m, Ord k)=>m (Dynamic t [k])

>   values <- forDyn joined (Prelude.map (fst) . elems)  -- Dynamc Simple
>   events <- forDyn joined (Prelude.map (snd) . elems) -- Dynamic [Event (SimpleWidgetEvent)]
>   events' <- forDyn events (leftmost) --  Dynamic t (Event t (SimpleWidgetEvent Int)) --  Dynamic (Event (SimpleWidgetEvent))
>   let events'' = switch $ current $ events'

>   display values
>   return values

>   where
>   makeDynMap childKeys parentEvent = do
>      temp <- mapDyn (\k-> fromList $ zip k $ repeat parentEvent) childKeys
>      temp'<- mapDyn mergeMap temp
>      let temp'' = switchPromptlyDyn temp'
>      return temp''
>   determineChildEvent (DeleteMe k)= k=:Nothing
>   determineChildEvent (FlashIndividual k) = k=:(Just (Right Flash))


> main = mainWidget $ el "div" $ do
>   growAndShrinkWidget' >>= display
