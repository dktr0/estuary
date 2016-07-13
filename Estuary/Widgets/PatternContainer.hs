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
import           Types.RequestMap

-- Haskell Imports
import           Control.Monad
import           Control.Monad.IO.Class
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
      soundE <- soundWidget initialSound R.never
--      soundWidget :: R.MonadWidget t m => Sound -> R.Event t SoundWidgetRequest -> m (R.Event t (SoundEvent, Sound))
      -- listen for add event
      addSound <- buttonWidget "add" appendAttrs
      -- tag sound that was
      soundDyn <- holdDyn (Empty,simpleSound "sn") soundE

      soundBeh <- return (current soundDyn)

      addSoundE <- return $ tag soundBeh addSound
      addSoundEE <- return $ (fmap (\(x,y) -> (Empty,y))) addSoundE

      --display soundDyn
      let events = [addSoundEE,soundE]

      -- leftmost (return whichever event fired) wrap sound in event
      soundTupleE <- return $ leftmost (events)

      display oneSound

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
  -- Behavior t [(k,(SoundEvent,Sound))]
  keySoundBL <- hold [] keySoundEL
  -- Event t ([old],[new])
  keySoundETL <- return $ attach (current keySoundDL) (updated keySoundDL)
  keySoundDTL <- holdDyn ([],[]) keySoundETL
  display keySoundDTL

  -- Need type Dynamic t ((oldKey :: Int ,oldSoundEvent :: SoundEvent ,oldSound :: Sound),(newKey :: Int ,newSoundEvent :: SoundEvent ,newSound :: Sound))
  insSoundE <- return $ fmap (determineInsert) keySoundETL
  insSound <- return $ fmap (Types.SoundPattern.insert) insSoundE

  keyE <- return $ fmap (\[(k,(se,s))] -> k) keySoundEL
  keyD <- holdDyn 0 keyE
  garbageE <- garbageWidget
  remSoundE <- return $ tagDyn keyD garbageE
  remSound <- return $ (fmap Types.SoundPattern.delete) remSoundE

  updSoundE <- return $ tagDyn keySoundDTL event
  updSoundEE <- return $ fmap (determineUpdate) updSoundE
  updSound <- return $ (fmap Types.SoundPattern.update) updSoundEE

  allSoundsList <- foldDyn ($) initialPattern (leftmost $ [remSound, updSound, insSound])
  allSoundsMap <- forDyn allSoundsList convertToMap

  display allSoundsMap

  let contAttrsDyn = (constDyn $ determineContAttributes Empty)

  -- Event Listeners
  x <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
  y <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
  z <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
  _ <- R.performEvent_ $ return () <$ y

  let event = leftmost [ ClickE      <$ R.domEvent R.Click cont,
                         HoveroverE  <$ R.domEvent R.Mouseover cont,
                         DropE       <$ x]

  return ()
  where
    appendAttrs = Data.Map.fromList [("class","squarebutton"),("style","left: 20px; bottom: 20px;")]

determineInsert :: ([(Int,(SoundEvent,Sound))],[(Int,(SoundEvent,Sound))]) -> Maybe (Sound,Int)
determineInsert ([],[]) = Nothing
determineInsert (x,[]) = Nothing
determineInsert ([],y) = Nothing
determineInsert ([(ok,(oe,os))],[(nk,(ne,ns))]) = if (oe == DropE && ne == DragendE) then Just(ns,ok)
                                                  else if (ne == Empty) then Just(ns,nk)
                                                  else (Nothing)

determineUpdate :: [(Int,(SoundEvent,Sound))] -> Maybe (Sound,Int)
determineUpdate [] = Nothing
determineUpdate [(nk,(ne,ns))] = if (ne == ClickE) then Just(ns,nk)
                                                   else Nothing

determineInsertReq :: ([(Int,(SoundEvent,Sound))],[(Int,(SoundEvent,Sound))]) -> Maybe (Int.Int,SoundWidgetRequest)
determineInsertReq ([],[]) = Nothing
determineInsertReq (x,[]) = Nothing
determineInsertReq ([],y) = Nothing
determineInsertReq ([(ok,(oe,os))],[(nk,(ne,ns))]) = if (oe == DropE && ne == DragendE) then Just(nk,ok,BecomeSound ns)
                                                     else if (ne == Empty) then Just(nk,ok,Add)
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

-- When there is an insert the current soundwidget has to be tagged for deletion
