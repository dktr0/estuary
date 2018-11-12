{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}

module Estuary.Reflex.POC where

import Control.Monad.IO.Class

import GHCJS.Marshal
import GHCJS.Marshal.Pure

import Estuary.Reflex.Router

import Reflex.Dom

import GHC.Generics

data T
  = P1 String
  | P2 String
  deriving (Show, Eq, Generic, FromJSVal, ToJSVal)

poc :: (MonadWidget t m) => m ()
poc = do
  router (P1 "start") $ \x -> case x of
    P1 s -> do
      click <- button (show x)
      return $ (P2 s) <$ click
    P2 s -> do
      click <- button (show x)
      return $ (P1 s) <$ click