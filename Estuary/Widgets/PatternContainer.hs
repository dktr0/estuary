-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

module           Widgets.PatternContainer where

import           Sound.Tidal.Context as Tidal
import           Tidal.Utils
import           Widgets.HelperWidgets
import           Widgets.SoundWidget
import           Types.Sound
import           Types.SoundPattern

-- Haskell Imports
import           Data.Tuple (fst, snd)
import           Data.String
import           Data.Default
import           Data.Maybe
import           Data.Map
import           Data.List

-- Reflex Imports
-- Reflex Quick Reference                : https://github.com/ryantrinkle/reflex/blob/develop/Quickref.md
--import           Reflex as R
-- Reflex.Dom Quick Reference            : https://github.com/ryantrinkle/reflex-dom/blob/develop/Quickref.md
import           Reflex.Dom as R
-- Reflex.Dom.Widget.Basic Documentation : https://hackage.haskell.org/package/reflex-dom-0.2/docs/Reflex-Dom-Widget-Basic.html#v:DropTag
import           Reflex.Dom.Widget.Basic as R

import           Reflex.Dom.Class as R

-- GHCJS Imports
import           GHCJS.Types as GHCJS
import qualified GHCJS.DOM.Event  as GHCJS (IsEvent)
import qualified GHCJS.DOM.Element as GHCJS
import           GHCJS.DOM.EventM as GHCJS (preventDefault, stopPropagation, EventM)

-- Create the sound widget
patternContainerWidget :: R.MonadWidget t m => m ()
patternContainerWidget = mdo
  (cont, keySoundBE) <- elDynAttr' "div" contAttrsDyn $ mdo

    -- Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t (SoundEvent, Sound)) -> m (Behavior t (Map k Event t (SoundEvent, Sound)))
    keySoundBE <- listViewWithKey' allSoundsMap (\k oneSound -> mdo
      soundE <- soundWidget

      -- listen for add event
      addSound <- buttonWidget "add" appendAttrs
      addSoundE <- return $ tagDyn (constDyn (Empty,simpleSound "sn")) addSound

      let events = [soundE, addSoundE]

      -- leftmost (return whichever event fired) wrap sound in event
      soundTupleE <- return $ leftmost (events)

      return $ soundTupleE)
    return $ keySoundBE

  -- Behavior t (Event t Map k (SoundEvent, Sound))
  keySoundBEM <- return $ (fmap R.mergeMap) keySoundBE
  -- Event t (Map k (SoundEvent,Sound))
  keySoundEM <- return $ switch keySoundBEM
  -- Event t [(k, (SoundEvent,Sound))]
  keySoundEL <- return $ (fmap Data.Map.toList) keySoundEM
  -- Dynamic t [(k,(SoundEvent,Sound))]
  keySoundDL <- holdDyn [] keySoundEL
  -- Event t ([old],[new])
  keySoundETL <- return $ attach (current keySoundDL) (updated keySoundDL)
  keySoundDTL <- holdDyn ([],[]) keySoundETL
  display keySoundDTL

  -- Need type Dynamic t ((oldKey :: Int ,oldSoundEvent :: SoundEvent ,oldSound :: Sound),(newKey :: Int ,newSoundEvent :: SoundEvent ,newSound :: Sound))
  insSoundE <- return $ fmap (determineInsert) keySoundETL
  insSound <- return $ (fmap Types.SoundPattern.insert) insSoundE

  keyE <- return $ fmap (\[(k,(se,s))] -> k) keySoundEL
  keyD <- holdDyn 0 keyE
  garbageE <- garbageWidget
  remSoundE <- return $ tagDyn keyD garbageE
  remSound <- return $ (fmap Types.SoundPattern.delete) remSoundE

  allSoundsList <- foldDyn ($) initialPattern (leftmost $ [remSound, insSound])
  allSoundsMap <- forDyn allSoundsList convertToMap

  display allSoundsList
  display allSoundsMap

  let contAttrsDyn = (constDyn $ determineContAttributes Empty)

  return ()
  where
    appendAttrs = Data.Map.fromList [("class","squarebutton"),("style","left: 20px; bottom: 20px;")]

--  listViewWithKey :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t Info)) -> m (Event t (Map k Info))
--  [17:22] <ryantrinkle> you could have each one return an Event t ()
--  [17:22] <ryantrinkle> then, your list widget would collect those into Event t (Map k (Event t Info))
--  [17:22] <ryantrinkle> you can hold that (using Map.empty as the initial value)
--  [17:23] <ryantrinkle> which will give you Behavior t (Map k (Event t Info))
--  [17:23] <ryantrinkle> then, you can fmap mergeMap over that
--  [17:23] <ryantrinkle> giving you: Behavior t (Event t (Map k Info))
--  [17:23] <ryantrinkle> then switch
--  [17:23] <ryantrinkle> giving: Event t (Map k Info)
--  [17:24] <ryantrinkle> the keys of that map will be the items that want to be deleted :)

determineInsert :: ([(Int,(SoundEvent,Sound))],[(Int,(SoundEvent,Sound))]) -> Maybe (Sound,Int)
determineInsert ([],[]) = Nothing
determineInsert (x,[]) = Nothing
determineInsert ([],y) = Nothing
determineInsert ([(ok,(oe,os))],[(nk,(ne,ns))]) = if (oe == DropE && ne == DragendE) then Just(ns,nk)
                                                  else if (ne == Empty) then Just(ns,nk)
                                                  else (Nothing)


determineContAttributes :: SoundEvent -> Map String String
determineContAttributes soundEvent
        | soundEvent == ClickE     = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,30%); border: 3px solid black;")]
        | soundEvent == DragE      = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
        | soundEvent == DropE      = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
        | soundEvent == DragoverE  = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,30%); border: 1px solid black;")]
        | soundEvent == HoveroverE = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,30%); border: 1px solid black;")]
        | otherwise                = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
