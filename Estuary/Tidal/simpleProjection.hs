import Reflex
import Reflex.Dom
import Estuary.Tidal.Util
import Sound.Tidal.Context

main = mainWidget $ el "div" $ do
  inputField <- textArea
