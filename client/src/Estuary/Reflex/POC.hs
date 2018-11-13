{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Estuary.Reflex.POC where

import Control.Monad.IO.Class

import GHCJS.Marshal

import Estuary.Reflex.Router

import Reflex.Dom

import GHC.Generics

data T
  = P1 String
  | P2 String
  deriving (Show, Eq, Generic, FromJSVal, ToJSVal)

poc :: (MonadWidget t m) => m (Dynamic t String)
poc = do
  dynRouterData <- router (P1 "start") $ \x -> case x of
    P1 s -> do
      click <- button (show x)
      return $ ((P2 $ '2':s) <$ click, s)
    P2 s -> do
      click <- button (show x)
      return $ ((P1 $ '1':s) <$ click, s)
  mapDyn snd dynRouterData

main :: IO ()
main = 
  mainWidget $ do
    dynRouterData <- poc
    dynText dynRouterData