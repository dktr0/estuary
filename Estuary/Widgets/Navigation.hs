{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Navigation where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Foreign
import Estuary.Tidal.Types
import Estuary.Tutorials.Context
import qualified Estuary.Tutorials.Tutorial as T
import Estuary.Widgets.Generic
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Control.Monad (liftM)
import Data.Map
import Text.Read
import Text.JSON
import Data.Time.Clock

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Hint
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Terminal
import Estuary.Types.Context
import Estuary.Types.Language

import Estuary.Widgets.View
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term

import Estuary.Widgets.Sequencer

data Navigation =
  Splash |
  TutorialList |
  Tutorial T.Tutorial |
  Solo |
  Lobby |
  CreateEnsemblePage |
  Collaborate String


navigation :: MonadWidget t m => UTCTime -> Dynamic t Context -> Event t Command -> Event t [ServerResponse] ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint)
navigation now ctx commands wsDown = mdo
  let initialPage = page ctx commands wsDown now Splash
  let rebuild = fmap (page ctx commands wsDown now) navEvents
  w <- widgetHold initialPage rebuild
  values <- liftM joinDyn $ mapDyn (\(x,_,_,_)->x) w
  wsUp <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_,_)->x) w
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x,_)->x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,_,x)->x) w
  return (values,wsUp,hints)

page :: MonadWidget t m => Dynamic t Context -> Event t Command -> Event t [ServerResponse] -> UTCTime -> Navigation ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint,Event t Navigation)

page ctx _ wsDown _ Splash = do
  x <- liftM (TutorialList <$) $ el "div" $ dynButton =<< translateDyn Term.Tutorials ctx
  y <- liftM (Solo <$)  $ el "div" $ dynButton =<< translateDyn Term.Solo ctx
  z <- liftM (Lobby <$)  $ el "div" $ dynButton =<< translateDyn Term.Collaborate ctx
  -- t <- liftM (Test <$) $ button "test"
  let navEvents = leftmost [x,y,z,t]
  return (constDyn [],never,never,navEvents)

page ctx _ wsDown _ TutorialList = do
  el "div" $ text "Click on a button to select a tutorial interface:"
  bs <- sequence $ fmap (\b-> liftM ((Tutorial b) <$)  $ button $ show $ T.tutorialId b) $ elems tutorials --m [ Event t Navigation]
  -- t1 <- liftM (Tutorial  <$) $ el "div" $ button "Structure editing"
  -- t2 <- liftM (Tutorial "TidalCycles text editing" <$) $ el "div" $ button "TidalCycles text editing"
  back <- liftM (Splash <$) $ el "div" $ button  "<----"
  let navEvents = leftmost $ bs ++ [back]
  return (constDyn [],never,never,navEvents)

page ctx _ wsDown _ (Tutorial t) = do
  (dm, h) <- T.tutorialWidget t ctx
  patterns <- mapDyn (justStructures . elems) dm
  x <- liftM (Splash <$) $ el "div" $ button "<-- Back to tutorial list"
  return (patterns, never, h, x)

page ctx _ wsDown _ (Tutorial _) = do
  text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as a bug on Estuary's github site"
  x <- liftM (Splash <$) $ button "<----"
  return (constDyn [],never,never,x)

page ctx _ wsDown _ Solo = do
  (defMap,hints) <- viewInSoloWidget standardView
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (Splash <$) $ button "<----"
  return (patterns,never,hints,x)

page ctx _ wsDown _ Lobby = do
  requestEnsembleList <- liftM (GetEnsembleList <$) getPostBuild
  spaceList <- holdDyn [] $ fmapMaybe justEnsembleList wsDown
  join <- simpleList spaceList joinButton -- m (Dynamic t [Event t Navigation])
  join' <- mapDyn leftmost join -- m (Dynamic t (Event t Navigation))
  let join'' = switchPromptlyDyn join' -- Event t Navigation
  create <- liftM (CreateEnsemblePage <$) $ el "div" $ dynButton =<< translateDyn Term.CreateNewEnsemble ctx
  back <- liftM (Splash <$) $ el "div" $ button "<----"
  return (constDyn [],requestEnsembleList,never,leftmost [back,join'',create])

page ctx _ _ _ CreateEnsemblePage = do
  el "div" $ dynText =<< translateDyn Term.CreateNewEnsemble ctx
  el "div" $ dynText =<< translateDyn Term.CreateNewEnsembleNote ctx
  adminPwd <- el "div" $ do
    translateDyn Term.AdministratorPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"
  name <- el "div" $ do
    translateDyn Term.EnsembleName ctx >>= dynText
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  password <- el "div" $ do
    translateDyn Term.EnsemblePassword ctx >>= dynText
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  nameAndPassword <- combineDyn (,) name password
  confirm <- el "div" $ dynButton =<< translateDyn Term.Confirm ctx
  let createEnsemble = fmap (\(a,b) -> CreateEnsemble a b) $ tagDyn nameAndPassword confirm
  let authenticateAdmin = fmap Authenticate $ updated adminPwd
  cancel <- el "div" $ dynButton =<< translateDyn Term.Cancel ctx
  let serverRequests = leftmost [createEnsemble,authenticateAdmin]
  let navEvents = fmap (const Lobby) $ leftmost [cancel,() <$ createEnsemble]
  return (constDyn [], serverRequests, never, navEvents)

page ctx commands wsDown now (Collaborate w) = do
  (defMap,wsUp,hints) <- viewInEnsembleWidget w now commands wsDown
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (Lobby <$) $ button  "<----"
  return (patterns,wsUp,hints,x)

-- page ctx commands wsDown now (Test) = do
--   spat <- sequencer (Just "~") isgp never
--   -- spat' <- mapDyn (\(a,_,_)->a) spat
--   upPat <- sequencer (Just 0) iugp never
--   -- p <- sequencer (Just "~") isgp never >>= mapDyn (\(x,_,_)-> [UntransformedPattern  $ S x])
--   p <- combineDyn (\(s,_,_) (u,_,_) -> [TransformedPattern (Combine (S s) Merge) $ UntransformedPattern $ Up u]) spat upPat
--   debug $ updated p
--   return (p,never,never,never)
--   where
--     isgp = Layers (Live (ls,Once) L4) Inert
--     ls = fmap f ["bd"::String,"cp","dr","techno"]
--     f s = Group (Live (take 8 $ repeat (Blank Inert),Once) L4) Inert
--     iugp = Layers (Live (uls,Once) L4) Inert
--     uls = fmap uf [0::Float]
--     uf s = Group (Live (take 8 $ repeat (Atom 0 Inert Once),Once) L4) Inert



joinButton :: MonadWidget t m => Dynamic t String -> m (Event t Navigation)
joinButton x = do
  b <- clickableDivClass'' x "placeholderClass" ()
  return $ Collaborate <$> tagDyn x b

{-
tempoWidget :: MonadWidget t m => Event t [ServerResponse] -> m (Event t ServerRequest)
tempoWidget deltas = do
  text "CPS:"
  let delta' = fmap (Prelude.filter isCps) deltas
  let delta'' = fmapMaybe lastOrNothing delta'
  let delta''' = fmapMaybe getCps delta''
  let delta'''' = fmap show delta'''
  t <- textInput $ def & textInputConfig_setValue .~ delta''''
  let t' = fmapMaybe (readMaybe) $ _textInput_input t
  let edits = fmap (TempoChange "") t'
  return edits
-}
