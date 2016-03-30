{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Numeric (
    finalizeMeanDev
  , summarizeNumericState
  , updateMinimum
  , updateMaximum
  , updateMeanDev
  , updateNumericState
  ) where

import           Control.Lens ((%~), (^.))

import           P

import           Warden.Data

updateMinimum :: Real a
              => Minimum -> a -> Minimum
updateMinimum !acc x =
  let x' = (Minimum . fromRational . toRational) x
  in acc <> x'
{-# INLINE updateMinimum #-}

updateMaximum :: Real a
              => Maximum -> a -> Maximum
updateMaximum !acc x =
  let x' = (Maximum . fromRational . toRational) x
  in acc <> x'
{-# INLINE updateMaximum #-}

-- Numerically-stable mean and variance.
--
-- \( \frac{1}{n} \sum_{x \in X} x \equiv M_1 = X_1, M_k = M_{k-1} + \frac{(X_k - M_{k-1})}{k} \)
updateMeanDev :: Real a
              => MeanDevAcc -> a -> MeanDevAcc
updateMeanDev !macc x =
  let x' = (fromRational . toRational) x in case macc of
  MeanDevInitial ->
    let i = Count 1
        m = MeanAcc 0
        s = Nothing
    in update' m s i x'
  (MeanDevAcc m s i) ->
    update' m s i x'
  where
    update' (MeanAcc m) s (Count i) v =
      let delta = v - m
          m'    = MeanAcc $ m + delta / (fromIntegral i)
          i'    = Count $ i + 1
          s'    = case s of
                    Nothing         -> Just $ Variance 0
                    Just (Variance var) -> Just . Variance $ var + delta * (v - (unMeanAcc m'))
      in MeanDevAcc m' s' i'
{-# INLINE updateMeanDev #-}

finalizeMeanDev :: MeanDevAcc -> (Mean, StdDev)
finalizeMeanDev MeanDevInitial = (NoMean, NoStdDev)
finalizeMeanDev (MeanDevAcc _ Nothing _) = (NoMean, NoStdDev)
finalizeMeanDev (MeanDevAcc mn (Just var) _) = (Mean (unMeanAcc mn), fromVariance var)

-- FIXME: median
updateNumericState :: Real a
                   => NumericState -> a -> NumericState
updateNumericState acc x =
    (stateMinimum %~ (flip updateMinimum x))
  . (stateMaximum %~ (flip updateMaximum x))
  . (stateMeanDev %~ (flip updateMeanDev x))
  $!! acc
{-# INLINE updateNumericState #-}

summarizeNumericState :: NumericState -> NumericSummary
summarizeNumericState st =
  let (mn, stddev) = finalizeMeanDev $ st ^. stateMeanDev in
  NumericSummary
    (st ^. stateMinimum)
    (st ^. stateMaximum)
    mn
    stddev
    NoMedian
