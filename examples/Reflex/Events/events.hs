import Reflex.Dom
import Reflex
import Reflex.Dom.Widget.Basic

main = mainWidget $ el "div" $ do
  --onClickText clickMe
  text "poop"

-- Return domEvent result
clickMe :: (MonadWidget t m) => String -> m (Event t ())
clickMe name = do
  (e, _) <- el "div" text (name)
  return $ domEvent Click e

-- Display text on event fire
onClickText :: MonadWidget t m => m (Event t String) -> m ()
onClickText event = el "div" $ do
  textShowDyn <- holdDyn "No Click" event
  dynText textShowDyn

-- Add to counter on event fire
--onClickCount :: MonadWidget t m => (Event t ()) -> m ()
--onClickCount event = el "div" $ do

--Switch div attributes on event fire
--onClickAttr :: MonadWidget t m => (Event t ()) -> m ()
--onClickAttr event = elDynAttr' "button" dynAttrs (text "")

-- Toggle display on event fire
--onClickToggle :: MonadWidget t m => (Event t ()) -> m ()
--onClickToggle event = el "div" $ do
