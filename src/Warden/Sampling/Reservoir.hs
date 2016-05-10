{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Sampling.Reservoir(
    combineReservoirAcc
  , concatMutable
  , finalizeReservoirAcc
  , newReservoir
  , updateReservoirAcc
  ) where

import           Control.Monad.Primitive (PrimMonad(..))

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

import           P

import           System.IO (IO)
import           System.Random.MWC (Gen, uniformR)
import           System.Random.MWC.Distributions (uniformShuffleM)

import           Warden.Data.Row
import           Warden.Data.Sampling
import           Warden.Data.Sampling.Reservoir

newReservoir :: ReservoirSize -> IO Reservoir
newReservoir (ReservoirSize n) =
  Reservoir <$> MVU.new n

-- | If we have less than the desired final number of elements (inferred from
-- the vector size) we take the current value and write it to the end of the
-- sample. If we have at least the desired final number of elements, we (with
-- probability decreasing with the number of elements seen) replace a random
-- element in the sample with the current value.
updateReservoirAcc :: Gen (PrimState IO)
                   -> ReservoirSize
                   -> RowCount
                   -> ReservoirAcc
                   -> Double
                   -> IO ReservoirAcc
updateReservoirAcc gen rs seen NoReservoirAcc x = {-# SCC updateReservoirAcc #-} do
  r <- newReservoir rs
  updateReservoirAcc' gen seen r initialSampleCount x
updateReservoirAcc gen _rs seen (ReservoirAcc r c) x = {-# SCC updateReservoirAcc #-}
  updateReservoirAcc' gen seen r c x
{-# INLINE updateReservoirAcc #-}

updateReservoirAcc' :: Gen (PrimState IO)
                    -> RowCount
                    -> Reservoir
                    -> SampleCount
                    -> Double
                    -> IO ReservoirAcc
updateReservoirAcc' gen seen (Reservoir v) c x = {-# SCC updateReservoirAcc' #-}
  let target = MVU.length v
      c' = unSampleCount c
      seen' = unRowCount seen in
  if c' < target
    then do
      MVU.unsafeWrite v c' x
      pure $! ReservoirAcc (Reservoir v) (SampleCount $! c' + 1)
    else do
      u <- uniformR (0 :: Int, fromIntegral seen') gen
      when (u < target) $
        MVU.unsafeWrite v u x
      pure $! ReservoirAcc (Reservoir v) c
{-# INLINE updateReservoirAcc' #-}

-- | Join two samples to get a new sample of up to the provided 'ReservoirSize',
-- consisting of elements drawn uniformly from the union of the two original
-- samples.
combineReservoirAcc :: Gen (PrimState IO)
                    -> ReservoirSize
                    -> ReservoirAcc
                    -> ReservoirAcc
                    -> IO ReservoirAcc
combineReservoirAcc _g _rs NoReservoirAcc NoReservoirAcc =
  pure NoReservoirAcc
combineReservoirAcc _g _rs NoReservoirAcc y =
  pure y
combineReservoirAcc _g _rs x NoReservoirAcc =
  pure x
combineReservoirAcc g (ReservoirSize sz) (ReservoirAcc r1 sc1) (ReservoirAcc r2 sc2) =
  let sc1' = unSampleCount sc1
      sc2' = unSampleCount sc2
      desired = min sz (sc1' + sc2') in do
  pool <- (unReservoir r1) `concatMutable` (unReservoir r2)
  uniformShuffleM pool g
  let r = MVU.unsafeSlice 0 desired pool
  pure $! ReservoirAcc (Reservoir r) (SampleCount desired)

-- | Concatenate two mutable vectors with minimal copying by growing the
-- first vector.
concatMutable :: MVU.IOVector Double -> MVU.IOVector Double -> IO (MVU.IOVector Double)
concatMutable xs ys =
  let nx = MVU.length xs
      ny = MVU.length ys in do
  zs <- MVU.unsafeGrow xs ny
  mapM_ (\ix -> MVU.unsafeWrite zs (ix + nx) =<< MVU.unsafeRead ys ix) [0..(ny-1)]
  pure zs

finalizeReservoirAcc :: ReservoirAcc -> IO Sample
finalizeReservoirAcc NoReservoirAcc =
  pure NoSample
finalizeReservoirAcc (ReservoirAcc (Reservoir v) _sc) =
  fmap Sample $ VU.freeze v
