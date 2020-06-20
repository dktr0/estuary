module Estuary.Types.TranslatableText where

import Data.Text (Text)
import Data.Map
import Estuary.Types.Language

type TranslatableText = Map Language Text

translateText :: TranslatableText -> Language -> Text
translateText t l = findWithDefault (head $ elems t) l t
