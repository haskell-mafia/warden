{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Warden.Sampling.Reservoir(
    finalizeReservoir
  , newReservoirAcc
  , updateReservoirAcc
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

updateReservoirAcc :: Gen (PrimState IO)
                   -> RowCount
                   -> ReservoirAcc
                   -> SampleCount
                   -> Double
                   -> IO SampleCount
updateReservoirAcc gen seen (ReservoirAcc v) c x =
  let target = MVU.length v
      c' = unSampleCount c
      seen' = unRowCount seen in
  if c' < target
    then do
      MVU.write v c' x
      pure . SampleCount $! c' + 1
    else do
      u <- uniformR (0 :: Int, fromIntegral seen') gen
      when (u < target) $
        MVU.write v u x
      pure c
#ifndef NOINLINE
{-# INLINE updateReservoirAcc #-}
#endif

finalizeReservoir :: ReservoirAcc -> IO Sample
finalizeReservoir (ReservoirAcc v) =
  fmap Sample $ VU.freeze v
