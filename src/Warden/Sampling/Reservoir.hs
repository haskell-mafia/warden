{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Warden.Sampling.Reservoir(
    finalizeReservoir
  , newReservoirAcc
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

import           P

import           System.IO (IO)

import           Warden.Data.Sampling
import           Warden.Data.Sampling.Reservoir

newReservoirAcc :: ReservoirSize -> IO ReservoirAcc
newReservoirAcc (ReservoirSize n) =
  ReservoirAcc <$> MVU.new n

finalizeReservoir :: ReservoirAcc -> IO Sample
finalizeReservoir (ReservoirAcc v) =
  fmap Sample $ VU.freeze v
