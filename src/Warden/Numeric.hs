{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Numeric (
    combineFieldNumericState
  , combineMeanAcc
  , combineMeanDevAcc
  , combineNumericState
  , combineStdDevAcc
  , sampleMedian
  , summarizeFieldNumericState
  , summarizeNumericState
  , unsafeMedian
  , updateMinimum
  , updateMaximum
  , updateMeanDev
  , updateNumericState
  ) where

import           Control.Lens ((%~), (^.))

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as VU

import           P

import           System.IO (IO)

import           Warden.Data
import           Warden.Sampling.Reservoir

updateMinimum :: Minimum -> Double -> Minimum
updateMinimum !acc x = {-# SCC updateMinimum #-}
  acc <> (Minimum x)
{-# INLINE updateMinimum #-}

updateMaximum :: Maximum -> Double -> Maximum
updateMaximum !acc x = {-# SCC updateMaximum #-}
  acc <> (Maximum x)
{-# INLINE updateMaximum #-}

-- | Minimal-error mean and standard deviation with Welford's method.
--
-- From Knuth (TAoCP v2, Seminumerical Algorithms, p232).
--
-- \( \frac{1}{n} \sum_{x \in X} x \equiv M_1 = X_1, M_k = M_{k-1} + \frac{(X_k - M_{k-1})}{k} \)
updateMeanDev :: MeanDevAcc -> Double -> MeanDevAcc
updateMeanDev !macc x = {-# SCC updateMeanDev #-} case macc of
  MeanDevInitial ->
    let i = KAcc 1
        m = MeanAcc 0
        s = NoStdDevAcc
    in update' m s i x
  (MeanDevAcc m s i) ->
    update' m s i x
  where
    update' (MeanAcc m) s (KAcc i) v =
      let delta = v - m
          m'    = MeanAcc $ m + delta / (fromIntegral i)
          i'    = KAcc $ i + 1
          s'    = case s of
                    NoStdDevAcc ->
                      MStdDevAcc $ StdDevAcc 0
                    MStdDevAcc (StdDevAcc sda) ->
                      MStdDevAcc . StdDevAcc $!! sda + (delta * (v - (unMeanAcc m')))
      in MeanDevAcc m' s' i'
{-# INLINE updateMeanDev #-}

updateNumericState :: NumericState -> Double -> NumericState
updateNumericState acc x = {-# SCC updateNumericState #-}
    (stateMinimum %~ (flip updateMinimum x))
  . (stateMaximum %~ (flip updateMaximum x))
  . (stateMeanDev %~ (flip updateMeanDev x))
  $!! acc
{-# INLINE updateNumericState #-}

-- FIXME: this might commute error, requires further thought.
combineMeanDevAcc :: MeanDevAcc -> MeanDevAcc -> MeanDevAcc
combineMeanDevAcc MeanDevInitial MeanDevInitial = {-# SCC combineMeanDevAcc #-}
  MeanDevInitial
combineMeanDevAcc MeanDevInitial md2 = {-# SCC combineMeanDevAcc #-}
  md2
combineMeanDevAcc md1 MeanDevInitial = {-# SCC combineMeanDevAcc #-}
  md1
combineMeanDevAcc (MeanDevAcc mu1 s1 c1) (MeanDevAcc mu2 s2 c2) = {-# SCC combineMeanDevAcc #-}
  let mu' = combineMeanAcc (mu1, c1) (mu2, c2)
      sda' = combineStdDevAcc mu' (mu1, s1, c1) (mu2, s2, c2)
      -- KAccs are off-by-one from the actual number of values seen, so
      -- subtract one from the sum to prevent it becoming off-by-two.
      c' = c1 + c2 - (KAcc 1) in
  MeanDevAcc mu' sda' c'
{-# INLINE combineMeanDevAcc #-}

-- | Combine stddev accumulators of two subsets by converting to variance
-- (pretty cheap), combining the variances (less cheap), and converting back.
--
-- There's almost certainly a better way to do this.
combineStdDevAcc :: MeanAcc -- ^ Combined mean.
                -> (MeanAcc, MStdDevAcc, KAcc) -- ^ First subset.
                -> (MeanAcc, MStdDevAcc, KAcc) -- ^ Second subset.
                -> MStdDevAcc
combineStdDevAcc _ (_, NoStdDevAcc, _) (_, NoStdDevAcc, _) = {-# SCC combineStdDevAcc #-}
  NoStdDevAcc
combineStdDevAcc _ (_, MStdDevAcc (StdDevAcc s1), _) (_, NoStdDevAcc, _) = {-# SCC combineStdDevAcc #-}
  MStdDevAcc $ StdDevAcc s1
combineStdDevAcc _ (_, NoStdDevAcc, _) (_, MStdDevAcc (StdDevAcc s2), _) = {-# SCC combineStdDevAcc #-}
  MStdDevAcc $ StdDevAcc s2
combineStdDevAcc muHat (mu1, MStdDevAcc sda1, c1) (mu2, MStdDevAcc sda2, c2) = {-# SCC combineStdDevAcc #-}
  let var1 = varianceFromStdDevAcc c1 sda1
      var2 = varianceFromStdDevAcc c2 sda2 in
  MStdDevAcc . stdDevAccFromVariance (c1 + c2 - (KAcc 1)) $
    combineVariance muHat (mu1, var1, c1) (mu2, var2, c2)
{-# INLINE combineStdDevAcc #-}

-- | Combine variances of two subsets of a sample (that is, exact variance of
-- datasets rather than estimate of variance of population).
--
-- The derivation of this formula is in the Numerics section of the
-- documentation.
combineVariance :: MeanAcc -- ^ Combined mean.
                -> (MeanAcc, Variance, KAcc) -- ^ First subset.
                -> (MeanAcc, Variance, KAcc) -- ^ Second subset.
                -> Variance
combineVariance (MeanAcc muHat) (MeanAcc mu1, Variance var1, KAcc c1) (MeanAcc mu2, Variance var2, KAcc c2) = {-# SCC combineVariance #-}
  let t1 = (c1' * var1) + (c1' * mu1 * mu1)
      t2 = (c2' * var2) + (c2' * mu2 * mu2) in
  Variance $ ((t1 + t2) * (1.0 / (c1' + c2'))) - (muHat * muHat)
  where
    c1' = fromIntegral $ c1 - 1

    c2' = fromIntegral $ c2 - 1
{-# INLINE combineVariance #-}

-- | Combine mean of two subsets, given subset means and size.
combineMeanAcc :: (MeanAcc, KAcc) -> (MeanAcc, KAcc) -> MeanAcc
combineMeanAcc (MeanAcc mu1, KAcc c1) (MeanAcc mu2, KAcc c2) = {-# SCC combineMeanAcc #-}
  let c1' = fromIntegral $ c1 - 1
      c2' = fromIntegral $ c2 - 1 in
  MeanAcc $ ((mu1 * c1') + (mu2 * c2')) / (c1' + c2')
{-# INLINE combineMeanAcc #-}

-- FIXME: not associative
combineNumericState :: NumericState -> NumericState -> NumericState
combineNumericState ns1 ns2 = {-# SCC combineNumericState #-}
    (stateMinimum %~ (<> (ns1 ^. stateMinimum)))
  . (stateMaximum %~ (<> (ns1 ^. stateMaximum)))
  . (stateMeanDev %~ (combineMeanDevAcc (ns1 ^. stateMeanDev)))
  $!! ns2
{-# INLINE combineNumericState #-}

combineFieldNumericState :: FieldNumericState -> FieldNumericState -> FieldNumericState
combineFieldNumericState NoFieldNumericState NoFieldNumericState = {-# SCC combineFieldNumericState #-}
  NoFieldNumericState
combineFieldNumericState NoFieldNumericState fns2 = {-# SCC combineFieldNumericState #-}
  fns2
combineFieldNumericState fns1 NoFieldNumericState = {-# SCC combineFieldNumericState #-}
  fns1
combineFieldNumericState (FieldNumericState ns1) (FieldNumericState ns2) = {-# SCC combineFieldNumericState #-}
  FieldNumericState $ V.zipWith combineNumericState ns1 ns2
{-# INLINE combineFieldNumericState #-}

-- | Exact median of sample data. Unsafe, don't call this directly
-- unless you know what you're doing.
unsafeMedian :: VU.Vector Double -> Double
unsafeMedian v =
  let n = VU.length v
      v' = VU.modify Intro.sort v in
    case n `mod` 2 of
      0 ->
        let right = n `div` 2
            left = right - 1 in
        ((v' ! left) + (v' ! right)) / 2
      _ ->
        v' ! (n `div` 2)

sampleMedian :: Sample -> Median
sampleMedian NoSample = NoMedian
sampleMedian (Sample v) =
  let n = VU.length v in
  if n < 2
    then
      NoMedian
    else
      Median $ unsafeMedian v

summarizeNumericState :: NumericState -> Sample -> NumericSummary
summarizeNumericState st smpl =
  if st == initialNumericState
    -- We didn't see any numeric fields, so there's nothing to summarize.
    then NoNumericSummary
    else let (mn, stddev) = finalizeMeanDev $ st ^. stateMeanDev in
      NumericSummary
        (st ^. stateMinimum)
        (st ^. stateMaximum)
        mn
        stddev
        (sampleMedian smpl)

summarizeFieldNumericState :: FieldNumericState
                           -> FieldReservoirAcc
                           -> IO NumericFieldSummary
summarizeFieldNumericState NoFieldNumericState _ =
  pure NoNumericFieldSummary
summarizeFieldNumericState (FieldNumericState ss) NoFieldReservoirAcc =
  if V.null ss
    then
      pure NoNumericFieldSummary
    else
      pure . NumericFieldSummary $
        V.zipWith summarizeNumericState ss (V.replicate (V.length ss) NoSample)
summarizeFieldNumericState (FieldNumericState ss) (FieldReservoirAcc fra) = do
  samples <- V.mapM finalizeReservoirAcc fra
  if V.null ss
    then
      pure NoNumericFieldSummary
    else
      pure . NumericFieldSummary $ V.zipWith summarizeNumericState ss samples
