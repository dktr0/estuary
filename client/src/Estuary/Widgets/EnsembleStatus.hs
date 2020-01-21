{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.EnsembleStatus where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow

import Estuary.Types.Context
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.Participant
import Estuary.Widgets.Editor

ensembleStatusWidget :: MonadWidget t m => Editor t m ()
ensembleStatusWidget = do

  -- extract data about ensemble from the context, filtering out all duplicate events
  -- TODO: still need to add filtering of duplicate events...
  ctx <- askContext
  let ensC = fmap ensembleC ctx
  let ens = fmap ensemble ensC
  let ensName = fmap ensembleName ens -- Dynamic t Text
  let ensParticipants = fmap participants ens -- Dynamic t (Map.Map Text Participant)
  let anonymous = fmap anonymousParticipants ens -- Dynamic t Int

  -- display name of ensemble
  liftR $ divClass "ensemble-name code-font" $ do
    text "Ensemble: "
    dynText ensName

  liftR2 (divClass "statusElementsWrapper") $ do
    -- display list of non-anonymous participants and locations
      liftR $ divClass "statusElementWrapper code-font" $ do
         divClass "statusElementName" $ text "Participants: "
         divClass "statusElement" $ listWithKey ensParticipants ensembleParticipantWidget

      -- display count of anonymous participants
      liftR $ divClass "statusElementWrapper code-font" $ do
        divClass "statusElementName" $ text "Anonymous Participants: "
        divClass "statusElement" $ dynText $ fmap showt anonymous

  return ()

ensembleParticipantWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
ensembleParticipantWidget name part = elClass "div" "" $ do
  text name
  text " ("
  dynText $ fmap location part
  text ") "
