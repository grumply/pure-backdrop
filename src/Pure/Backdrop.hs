{-# language NamedFieldPuns, LambdaCase, BlockArguments, TypeApplications, DataKinds, KindSignatures, RankNTypes, FlexibleInstances, OverloadedStrings, PostfixOperators, PolyKinds, ScopedTypeVariables, PartialTypeSignatures #-}
module Pure.Backdrop 
  ( Backdrop(..)
  , defaultBackdrop
  ) where

import Pure.Elm.Component
import Pure.Data.Txt as Txt (tail)

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat,Nat,natVal)

data Backdrop (milliseconds :: Nat) = Backdrop
  { lock :: Bool
  , withBackdrop :: View -> View
  }

defaultBackdrop :: Backdrop 300
defaultBackdrop = Backdrop False

instance (Theme (Backdrop millis), KnownNat millis) => Component (Backdrop millis) where
  data Msg (Backdrop millis) = Startup | Shutdown

  startup = [Startup]
  shutdown = [Shutdown]

  upon = \case
    Startup -> startup
    Shutdown -> shutdown

  view Backdrop { withBackdrop } _ =
    withBackdrop (Portal (coerce body) 
      (Div <| Themed @(Backdrop millis)))

startup :: Update n
startup (Backdrop { lock },_) mdl = do
  -- FIXME: if Backdrop changes while rendered, 
  -- the ModalOpen class might get left on <body>
  when lock do
    addThemeClass @ModalOpen (coerce body)
  pure mdl

shutdown :: Update n
shutdown (Backdrop { lock },_) mdl = do
  when lock do
    removeThemeClass @ModalOpen (coerce body)
  pure mdl

data ModalOpen
instance Theme ModalOpen where
  theme c =
    is c do
      overflow =: hidden

defaultBackdropStyles = do
  position =: fixed
  top =: 0
  left =: 0
  z-index =: 1000
  width =: 100vw
  height =: 100vh
  background-color =: black

instance KnownNat milliseconds => Theme (Backdrop milliseconds) where
  theme c = do
    let anim = Txt.tail c <> "_fade"
        m = fromIntegral (natVal (Proxy :: Proxy milliseconds))

    is c do
      defaultBackdropStyles
      opacity   =: 0
      animation =* [anim,m <> "ms",easein,forwards]

    atKeyframes anim do
      is (0%) do
        opacity =: 0

      is (100%) do
        opacity =: 0.85