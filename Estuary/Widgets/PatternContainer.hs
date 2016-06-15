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
  (cont, soundDynMap) <- elDynAttr' "div" contAttrsDyn $ mdo
    -- listWithKeyShallowDiff :: (Ord k, MonadWidget t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
    -- Display the given map of items (in key order) using the builder function provided, and update it with the given event.
    -- Nothing update entries will delete the corresponding children, and Just entries will create them if they do not exist or send an update event to them if they do.
    soundDynMap <- listWithKeyShallowDiff initRequestMap allSoundEventRequests buildSoundWidget
    return $ soundDynMap

  -- Convert Dynamic Map into behaviour to take snapshot Behaviour t (Map k Event t (SoundEvent,Sound))
  soundBehMapEvent <- return $ current soundDynMap
  -- Convert Behaviour Map of Events into type Behavior t (Event (Map k (SoundEvent,Sound)))
  soundBehEventMap <- return $ fmap R.mergeMap soundBehMapEvent
  -- Convert Behavior Event Map into Event Map
  soundEventMap <- return $ switch soundBehEventMap
  -- Convert Event Map into Event List Event t [k,(SoundEvent,Sound)]
  soundEventList <- return $ fmap Data.Map.toList soundEventMap
  -- Convert Event Map into Dynamic Map Dynamic t [k,(SoundEvent,Sound)]
  soundDynList <- holdDyn [] soundEventList
  -- Keep track of current and previous Events (Event t [Int,(SoundEvent,Sound)],Event t [Int,(SoundEvent,Sound)])
  oldNewSoundEventLists <- return $ attach (current soundDynList) (updated soundDynList)

  garbageE <- buttonWidget "delete" deleteAttrs

  keyE <- return $ fmap (\[(k,(se,s))] -> k) soundEventList
  keyD <- holdDyn 0 keyE

  -- Remove sound from list when drop event fires on garbage widget
  remSoundE <- return $ tagDyn keyD garbageE
  remSound <- return $ (fmap Types.SoundPattern.delete) remSoundE

  -- Need type Dynamic t ((oldKey :: Int,oldSoundEvent :: SoundEvent,oldSound :: Sound),(newKey :: Int,newSoundEvent :: SoundEvent,newSound :: Sound))
  insSoundE <- return $ fmap (determineInsert) oldNewSoundEventLists
  insSound <- return $ (fmap Types.SoundPattern.insert) insSoundE

  -- Run update on every click event
  updSoundE <- return $ fmap (determineUpdate) soundEventList
  updSound <- return $ (fmap Types.SoundPattern.update) updSoundE

  -- Maintain pattern
  allSoundsDynList <- foldDyn ($) initialPattern $ R.mergeWith (.) [remSound, insSound, updSound]
  --allSoundsEventList <- return $ updated allSoundsDynList
  --allSoundsEventMap <- return $ (fmap convertToMapMaybe) allSoundsEventList

  -- remReq
  remReq <- return $ (fmap Types.RequestMap.delete) remSoundE

  -- insReq
  insReqE <- return $ fmap (determineInsertReq) oldNewSoundEventLists
  insReq <- return $ (fmap Types.RequestMap.insert) insReqE

  -- adjReq
  adjReqE <- return $

  -- Maintain Requests
  allSoundsDynRequestsMap <- foldDyn ($) initialReqMap $ R.mergeWith (.) [remReq, insReq, adjReq]
  allSoundEventRequestsMap <- return $ updated allSoundsDynRequestsMap

  display allSoundsDynList

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
    deleteAttrs = Data.Map.fromList [("class","squarebutton"),("style","left: 20px; bottom: 20px;")]

buildSoundWidget :: MonadWidget t m => Int -> SoundWidgetRequest -> R.Event t SoundWidgetRequest -> m (R.Event t (SoundEvent,Sound))
buildSoundWidget key initSound request = mdo

  -- MakeUI elements and get their events
  soundWidgetEvent <- soundWidget initSound request
  addSoundButtonEvent <- buttonWidget "add" appendAttrs

  soundDyn <- holdDyn (Empty,simpleSound "bd") soundWidgetEvent
  addSound <- return $ tagDyn soundDyn addSoundButtonEvent
  addSoundE <- return $ (fmap (\(x,y) -> (Empty,y))) addSound

  -- leftmost (return whichever event fired) wrap sound in event
  soundTupleE <- return $ leftmost [addSoundE,soundWidgetEvent]
  return $ soundTupleE
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
