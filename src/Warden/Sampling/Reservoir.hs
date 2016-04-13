{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Warden.Sampling.Reservoir(
    combineReservoirAccs
  , finalizeReservoir
  , newReservoirAcc
  , updateReservoirAcc
  ) where

import           Control.Monad.Primitive (PrimMonad(..))

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

import           P

import           System.IO (IO)
import           System.Random.MWC (Gen, uniformR)

import           Warden.Data.Row
import           Warden.Data.Sampling
import           Warden.Data.Sampling.Reservoir
import           Warden.Random

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

combineReservoirAccs :: Gen (PrimState IO)
                     -> ReservoirSize
                     -> (SampleCount, ReservoirAcc)
                     -> (SampleCount, ReservoirAcc)
                     -> IO (SampleCount, ReservoirAcc)
combineReservoirAccs g (ReservoirSize sz) (SampleCount sc1, r1) (SampleCount sc2, r2) =
  let desired = min sz (sc1 + sc2) in do
  r1' <- VU.freeze $ unReservoirAcc r1
  r2' <- VU.freeze $ unReservoirAcc r2
  pool <- VU.thaw $ r1' VU.++ r2'
  uniformShuffle g pool
  let r = MVU.slice 0 (desired - 1) pool
  pure $ (SampleCount desired, ReservoirAcc r)

finalizeReservoir :: ReservoirAcc -> IO Sample
finalizeReservoir (ReservoirAcc v) =
  fmap Sample $ VU.freeze v
