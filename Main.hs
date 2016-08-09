module Main where

import Reflex
import Reflex.Dom


import Estuary.Tidal.Types
import Estuary.Reflex.Utility
--import Estuary.Widgets.Sound
--import Estuary.Widgets.SoundPattern

import Control.Monad (liftM)
import Sound.Tidal.Context
import Estuary.WebDirt.Stream
import Estuary.Page
import Estuary.Widgets.SpecificPattern

--import Estuary.Widgets.TransformedPattern

--main = mainWidget $ (simpleSWidget Blank  never)>>= display

main :: IO ()
main = do
  webDirt <- webDirtStream
  mainWidget $ multipage webDirt pages

pages = [("S-Specific Pattern",widgetToPage $ sContainerWidget (Estuary.Tidal.Types.S Blank) never),
  ("Pan-Specific Pattern", widgetToPage $ panContainerWidget (Pan $ Atom 0.5 Once) never),
  ("Sample and Pan Pattern", widgetToPage $ panSampleWidget (Estuary.Tidal.Types.S Blank) never)
  ]

-- pages :: MonadWidget t m => [m (Dynamic t ParamPattern)]
-- pages = [
--   ("trivialSoundPattern",widgetToPage trivialSoundPattern),
--   ("trivialTransformedPattern",widgetToPage trivialTransformedPattern),
--   ("Simple Container",widgetToPage multiTextWidget),
--   ("soundPatternContainer",widgetToPage $ soundPatternContainer emptySoundPattern never),
--   ("Transformed Text Widget",widgetToPage $ transformedTextWidget emptyTransformedPattern never)
--   ]
