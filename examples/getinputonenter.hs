import Reflex
import Reflex.Dom
import Sound.Tidal.Context

main = mainWidget $ el "div" $ do
  pattern <- patternInput
  patternString <- mapDyn show pattern
  dynText patternString

patternInput :: MonadWidget t m => m (Dynamic t (Maybe String))
patternInput = do
  n <- textInput $ def & textInputConfig_inputType .~ {-Sound.Tidal.Pattern-}
                       & textInputConfig_initialValue .~ ""
  mapDyn readMay $ _textInput_value n

keypressEvent :: MonadHold t m => a -> Event t a -> m (Dynamic t a)
