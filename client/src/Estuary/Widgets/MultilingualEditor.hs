{-# LANGUAGE OverloadedStrings, FlexibleContexts #-} {-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.MultilingualEditor where

import Reflex
import Reflex.Dom
import Reflex.Dynamic

import Data.Text
import Data.Map as M
import Data.Maybe

import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Estuary.Types.Language
import Estuary.Types.TranslatableText

-- type TranslatableText = Map Language Text

translatableTextWidget ::  MonadWidget t m => Dynamic t (TranslatableText) -> W t m  (Variable t TranslatableText)
translatableTextWidget delta = mdo
  editorWidget <- multilingualEditorWidget (constDyn "hi") -- Event Text
  let insertTextF = fmap (\t -> insertNewTextIntoTranslatableText (English, t)) editorWidget
  let localF = mergeWith (.) [insertTextF] -- Event t (CalendarEvent -> CalendarEvent)--
  let localUpdates = attachWith (flip ($)) (current $ currentValue v) localF -- Event t CalendarEvent
  v <- variable delta localUpdates
  return v

inputHint :: Text -> Map Text Text
inputHint x = "placeholder" =: x

multilingualEditorWidget :: MonadWidget t m => Dynamic t Text ->  W t m  (Event t Text) -- debe ser solo Dynamic Text
multilingualEditorWidget t = mdo
  let inputHint' = fmap inputHint (constDyn $ "")
  editor <- textInputW inputHint' t
  return editor

languageDropdownMenu :: MonadWidget t m =>  W t m  (Dynamic t Language)
languageDropdownMenu = do
  englishMap <- translatableText (M.fromList [(English,"English"), (Español, "Ingles")]) -- Dynamic t Text
  spanishMap <- translatableText (M.fromList [(English,"Spanish"), (Español, "Español")]) -- Dynamic t Text
  let languageMap' = languageMap <$>  englishMap <*> spanishMap -- Dynamic t (Map Int Text)
  dd <- dropdown 0 languageMap' def -- Map k Text -> Dynamic t k -> m (Event t k)
  let selItem = result <$> value dd <*> languageMap' -- Dynamic t Text
  return selItem

languageMap :: Text -> Text -> M.Map Int Text
languageMap t1 t2 = M.fromList [(1, t1), (2, t2)]

result :: Int -> M.Map Int Text -> Language
result key lMap = textToLanguage $ fromJust (M.lookup key lMap)

textToLanguage :: Text -> Language
textToLanguage "English" = English
textToLanguage "Ingles" = English
textToLanguage "Spanish" = Español
textToLanguage "Español" = Español


insertNewTextIntoTranslatableText :: (Language, Text) -> TranslatableText -> TranslatableText
insertNewTextIntoTranslatableText (l, t) xs = insert l t xs -- TranslatableText

-- import Data.IntMap as IntMap
-- import Estuary.Widgets.W hiding (theme)
--
-- import Estuary.Widgets.MultilingualEditor
-- import Data.Map as M
-- import Estuary.Types.Language


-- main = mainWidget $ do
--   let initial = M.fromList [(English,"initial text"), (Español, "texto inicial")] -- :: TranslatableText
--   newText <- fmap ((M.fromList [(English,"another text"), (Español, "otro texto")]) <$) $ button "submit" -- Event TranslatableText
--   delta <- holdDyn initial newText -- Dynamic TranslatableText
--   v <- translatableTextWidget delta -- m (Variable t TranslatableText)
--   let cv = currentValue v -- Dynamic t TranslatableText
--   text "current value of widget: "
--   dynText $ fmap (T.pack . show) cv -- Dynamic t TranslatableText
--   localEdits' <- holdDyn "" $ fmap (T.pack . show) (localEdits v) -- Dynamic t Text
--   text "local edits only from widget: "
--   dynText localEdits' -- dynamic Text
