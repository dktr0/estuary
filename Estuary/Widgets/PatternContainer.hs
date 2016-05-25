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
  (cont, keySoundE) <- elDynAttr' "div" contAttrsDyn $ mdo
    -- Event t (k,a) :: Event t (k, Event t (SoundEvent, Sound))
    keySoundE <- selectViewListWithKey allSounds $ \k oneSound bool -> mdo
      soundE <- soundWidget

      -- listen for add event
      addSound <- buttonWidget "add" appendAttrs
      addSoundE <- attachDyn (constDyn Empty) addSound

      let events = [soundE, addSoundE]

      -- leftmost (return whichever event fired) wrap sound in event
      soundTupleE <- leftmost (events)

      return $ soundTupleE
    return $ keySoundE
  -- Dynamic t (k, Event t (SoundEvent,Sound))
  keySoundDyn <- holdDyn (Empty,never) keySoundE
  -- (Dynamic t k, Dynamic (Event t (SoundEvent,Sound))
  keySoundTuple <- splitDyn keySoundDyn
  -- Dynamic t k
  dynKey <- fst keySoundTuple
  -- Dynamic (Event t (SoundEvent,Sound))
  dynEvent <- snd keySoundTuple
  -- Event t (SoundEvent, Sound)
  soundTupE <- switchPromptlyDyn dynEvent
  -- Event t (k,(SoundEvent, Sound))
  soundTripE <- attachDyn dynKey soundTupE
  -- Dynamic t (k,(SoundEvent, Sound))
  soundTripDyn <- holdDyn (0,(Empty,silentSound)) soundTripE
  -- Event t ( (k,(SoundEvent, Sound)) , (k,(SoundEvent,Sound)) )
  oldNewTupE <- attach (current soundTripDyn) (updated soundTripDyn)
  -- Dynamic t ( (k,(SoundEvent,Sound)) , (k,(SoundEvent,Sound)) )
  oldNewTupDyn <- holdDyn ((0,(Empty,silentSound)),(0,(Empty,silentSound))) oldNewTupE
  -- Was it a drop event? If so use previous key for insert
      -- else use current key for insert

  -- Need type Dynamic t ((oldKey :: Int ,oldSoundEvent :: SoundEvent ,oldSound :: Sound),(newKey :: Int ,newSoundEvent :: SoundEvent ,newSound :: Sound))
  insSoundDyn <- forDyn oldNewTupDyn (\((ok,oe,os),(nk,ne,ns)) -> if (ne == DropE && oe == DragendE) then (nk,ok,os)
                                                                  else if (ne == Empty) then (nk,nk,ns)
                                                                  else (nk,nk,ns))
  insSoundE <- updated insSoundDyn
  let insSound = (fmap Types.SoundPattern.insert) insSoundE

  garbageE <- garbageWidget
  let remSoundE = tagDyn dynKey garbageE
  let remSound = (fmap Types.SoundPattern.delete) remSoundE

  allSounds <- foldDyn ($) initialContainer (leftmost $ [remSound, insSound])
  return ()

  contAttrsDyn <- determineContAttributes Empty
  return ()
  where
    appendAttrs = Data.Map.fromList [("class","squarebutton"),("style","left: 20px; bottom: 20px;")]

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
