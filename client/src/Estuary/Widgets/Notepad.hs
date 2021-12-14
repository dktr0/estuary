{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.Notepad where

import Reflex
import Reflex.Dom
import Control.Monad
import Data.Text (Text)
import Data.Sequence as Seq
import Data.Text as T
import GHCJS.DOM.EventM

import Estuary.Widgets.W
import Estuary.Types.Definition
import Estuary.Widgets.Reflex


notePadWidget :: MonadWidget t m => Dynamic t NotePad -> W t m (Variable t NotePad)
notePadWidget delta = divClass "fullWidthDiv" $ mdo
  let changes = currentValue v
  let pageNum = fmap fst changes
  let notes = fmap snd changes -- []
  let noteTupple = Seq.index <$> notes <*> pageNum -- (t,c)
  (titleEv,contentEv) <- titleContentWidget (fmap fst noteTupple) (fmap snd noteTupple) -- :: (Event t Text, Event t Text)
  let setNoteTitle' = fmap setNoteTitle titleEv -- :: m (Event t (Notepad -> Notepad))
  let setNoteContent' = fmap setNoteContent contentEv -- :: m (Event t (Notepad -> Notepad))
  -- add note
  addPageButton <- clickableDivClass "+" "prevNextButtons" -- :: m (Event t ())
  let addPage = addNote <$ addPageButton -- :: m (Event t (Notepad -> Notepad))
  -- erase note
  erasePageButton <- clickableDivClass "-" "prevNextButtons" -- :: m (Event t ())
  let erasePage = eraseNote <$ erasePageButton -- :: m (Event t (Notepad -> Notepad))
  -- prev page
  prevPageButton <- clickableDivClass "←" "prevNextButtons" -- :: m (Event t ())
  let prevPage = prevPageOfNote <$ prevPageButton -- :: m (Event t (Notepad -> Notepad))
  -- next page
  nextPageButton <- clickableDivClass "→" "prevNextButtons" -- :: m (Event t ())
  let nextPage = nextPageOfNote <$ nextPageButton -- :: m (Event t (Notepad -> Notepad))
  --
  let localEvs = mergeWith (.) [setNoteTitle',setNoteContent', addPage, erasePage, prevPage, nextPage]
  let localUpdates = attachWith (flip ($)) (current $ currentValue v) localEvs
  v <- variable delta localUpdates
  return v

-- & textAreaConfig_setValue .~ changes

titleContentWidget :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> m (Event t Text,Event t Text)
titleContentWidget t c = divClass "tutorialPageTitle code-font" $ do
  x <- textInput $ def & textInputConfig_setValue .~ (updated t)
                       & attributes .~ constDyn ("class" =: "name-tag-textarea code-font primary-color")
  let title = _textInput_input x -- :: Event t Text
  y <- textArea $ def & textAreaConfig_setValue .~ (updated c)
                      & attributes .~ constDyn ("class" =: "name-tag-textarea code-font primary-color primary-borders")
  let content = _textArea_input y
  return $ (title,content)

---------------------------------------------------------------------
-- PURE FUNCTIONS

------------------------------------------
-- FUNCTION THAT MOVES THROUGH THE NOTEPAD

nextPageOfNote :: NotePad -> NotePad
nextPageOfNote (currentPage,listOfNotes)
  | currentPage >= Prelude.length listOfNotes = (0,listOfNotes)
  | otherwise = ((currentPage+1),listOfNotes)

prevPageOfNote :: NotePad -> NotePad
prevPageOfNote (currentPage,listOfNotes)
  | currentPage <= 0 = ((Prelude.length listOfNotes), listOfNotes)
  | otherwise = ((currentPage-1),listOfNotes)


-- FUNCTIONS THAT CREATE AND ERASE NOTEPAGES

addNote :: NotePad -> NotePad
addNote notepad = do
  let note = ("","")
  (fst notepad, insertAt (fst notepad) note (snd notepad))

eraseNote :: NotePad -> NotePad
eraseNote notepad
  | Prelude.length (snd notepad) <= 1 = notepad
  | otherwise = (fst notepad, deleteAt (fst notepad) (snd notepad))


-- FUNCTIONS THAT CHANGES/UPDATES THE TITLE AND CONTENT OF A NOTEPAGE

setNoteContent :: Text -> NotePad -> NotePad
setNoteContent newC (currentPage,listOfNotes) = do
  let currentNote = getCurrentNotePage (currentPage,listOfNotes) -- :: NotePage
  let updatedNote = replaceContentInPage newC currentNote -- :: NotePage
  let updatedListOfNotes = update currentPage updatedNote listOfNotes -- :: NotePage
  (currentPage,updatedListOfNotes)

setNoteTitle :: Text -> NotePad -> NotePad
setNoteTitle newT (currentPage,listOfNotes) = do
  let currentNote = getCurrentNotePage (currentPage,listOfNotes)-- :: NotePage
  let updatedNote = replaceTitleInPage newT currentNote -- :: NotePage
  let updatedListOfNotes = update currentPage updatedNote listOfNotes -- :: NotePage
  (currentPage,updatedListOfNotes)

getCurrentNotePage :: NotePad -> NotePage
getCurrentNotePage (currentPage,listOfNotes) = listOfNotes `Seq.index` currentPage

replaceTitleInPage :: Text -> NotePage -> NotePage
replaceTitleInPage newT (currentT,currentC) = (newT,currentC)

replaceContentInPage :: Text -> NotePage -> NotePage
replaceContentInPage newC (currentT,currentC) = (currentT,newC)

--
