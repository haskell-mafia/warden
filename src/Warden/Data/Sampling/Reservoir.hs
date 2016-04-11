{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.Data.Sampling.Reservoir(
    ReservoirAcc(..)
  , ReservoirSize(..)
  ) where

import qualified Data.Vector.Unboxed as VU

import           GHC.Generics (Generic)

import           P

newtype ReservoirAcc =
  ReservoirAcc {
    unReservoirAcc :: VU.Vector Double
  } deriving (Eq, Show, Generic)

instance NFData ReservoirAcc

newtype ReservoirSize =
  ReservoirSize {
    unReservoirSize :: Int
  } deriving (Eq, Show, Generic)

instance NFData ReservoirSize
