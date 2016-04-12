{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

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
updateMinimum !acc x =
  acc <> (Minimum x)
#ifndef NOINLINE
{-# INLINE updateMinimum #-}
#endif

updateMaximum :: Maximum -> Double -> Maximum
updateMaximum !acc x =
  acc <> (Maximum x)
#ifndef NOINLINE
{-# INLINE updateMaximum #-}
#endif

-- | Minimal-error mean and standard deviation with Welford's method.
--
-- From Knuth (TAoCP v2, Seminumerical Algorithms, p232).
--
-- \( \frac{1}{n} \sum_{x \in X} x \equiv M_1 = X_1, M_k = M_{k-1} + \frac{(X_k - M_{k-1})}{k} \)
updateMeanDev :: MeanDevAcc -> Double -> MeanDevAcc
updateMeanDev !macc x = case macc of
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
#ifndef NOINLINE
{-# INLINE updateMeanDev #-}
#endif

updateNumericState :: NumericState -> Double -> NumericState
updateNumericState acc x =
    (stateMinimum %~ (flip updateMinimum x))
  . (stateMaximum %~ (flip updateMaximum x))
  . (stateMeanDev %~ (flip updateMeanDev x))
  $!! acc
#ifndef NOINLINE
{-# INLINE updateNumericState #-}
#endif

-- FIXME: this might commute error, requires further thought.
combineMeanDevAcc :: MeanDevAcc -> MeanDevAcc -> MeanDevAcc
combineMeanDevAcc MeanDevInitial MeanDevInitial = MeanDevInitial
combineMeanDevAcc MeanDevInitial md2 = md2
combineMeanDevAcc md1 MeanDevInitial = md1
combineMeanDevAcc (MeanDevAcc mu1 s1 c1) (MeanDevAcc mu2 s2 c2) =
  let mu' = combineMeanAcc (mu1, c1) (mu2, c2)
      sda' = combineStdDevAcc mu' (mu1, s1, c1) (mu2, s2, c2)
      -- KAccs are off-by-one from the actual number of values seen, so
      -- subtract one from the sum to prevent it becoming off-by-two.
      c' = c1 + c2 - (KAcc 1) in
  MeanDevAcc mu' sda' c'
#ifndef NOINLINE
{-# INLINE combineMeanDevAcc #-}
#endif

-- | Combine stddev accumulators of two subsets by converting to variance
-- (pretty cheap), combining the variances (less cheap), and converting back.
--
-- There's almost certainly a better way to do this.
combineStdDevAcc :: MeanAcc -- ^ Combined mean.
                -> (MeanAcc, MStdDevAcc, KAcc) -- ^ First subset.
                -> (MeanAcc, MStdDevAcc, KAcc) -- ^ Second subset.
                -> MStdDevAcc
combineStdDevAcc _ (_, NoStdDevAcc, _) (_, NoStdDevAcc, _) =
  NoStdDevAcc
combineStdDevAcc _ (_, MStdDevAcc (StdDevAcc s1), _) (_, NoStdDevAcc, _) =
  MStdDevAcc $ StdDevAcc s1
combineStdDevAcc _ (_, NoStdDevAcc, _) (_, MStdDevAcc (StdDevAcc s2), _) =
  MStdDevAcc $ StdDevAcc s2
combineStdDevAcc _muHat (mu1, MStdDevAcc sda1, c1) (mu2, MStdDevAcc sda2, c2) =
  MStdDevAcc combinedSOS
  where
    combinedSOS =
      let sda1' = unStdDevAcc sda1
          mu1' = unMeanAcc mu1
          c1' = fromIntegral $ unKAcc c1
          sda2' = unStdDevAcc sda2
          mu2' = unMeanAcc mu2
          c2' = fromIntegral $ unKAcc c2
          delta = mu1' - mu2' in
      StdDevAcc $ sda1' + sda2' + (delta * delta) * ((c1' * c2') / (c1' + c2'))
#ifndef NOINLINE
{-# INLINE combineStdDevAcc #-}
#endif

-- | Combine mean of two subsets, given subset means and size.
combineMeanAcc :: (MeanAcc, KAcc) -> (MeanAcc, KAcc) -> MeanAcc
combineMeanAcc (MeanAcc mu1, KAcc c1) (MeanAcc mu2, KAcc c2) =
  let c1' = fromIntegral $ c1 - 1
      c2' = fromIntegral $ c2 - 1 in
  MeanAcc $ ((mu1 * c1') + (mu2 * c2')) / (c1' + c2')
#ifndef NOINLINE
{-# INLINE combineMeanAcc #-}
#endif

-- FIXME: not associative
combineNumericState :: NumericState -> NumericState -> NumericState
combineNumericState ns1 ns2 =
    (stateMinimum %~ (<> (ns1 ^. stateMinimum)))
  . (stateMaximum %~ (<> (ns1 ^. stateMaximum)))
  . (stateMeanDev %~ (combineMeanDevAcc (ns1 ^. stateMeanDev)))
  $!! ns2
#ifndef NOINLINE
{-# INLINE combineNumericState #-}
#endif

combineFieldNumericState :: FieldNumericState -> FieldNumericState -> FieldNumericState
combineFieldNumericState NoFieldNumericState NoFieldNumericState =
  NoFieldNumericState
combineFieldNumericState NoFieldNumericState fns2 =
  fns2
combineFieldNumericState fns1 NoFieldNumericState =
  fns1
combineFieldNumericState (FieldNumericState ns1) (FieldNumericState ns2) =
  FieldNumericState $ V.zipWith combineNumericState ns1 ns2
#ifndef NOINLINE
{-# INLINE combineFieldNumericState #-}
#endif

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
