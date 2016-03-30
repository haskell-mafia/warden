{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Numeric (
    finalizeMeanDev
  , updateMinimum
  , updateMaximum
  , updateMeanDev
  , updateNumericState
  ) where

import           Control.Lens ((%~))

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
        m = Mean 0
        s = Nothing
    in update' m s i x'
  (MeanDevAcc m s i) ->
    update' m s i x'
  where
    update' (Mean m) s (Count i) v =
      let delta = v - m
          m'    = Mean $ m + delta / (fromIntegral i)
          i'    = Count $ i + 1
          s'    = case s of
                    Nothing         -> Just $ Variance 0
                    Just (Variance var) -> Just . Variance $ var + delta * (v - (getMean m'))
      in MeanDevAcc m' s' i'
{-# INLINE updateMeanDev #-}

finalizeMeanDev :: MeanDevAcc -> Maybe (Mean, StdDev)
finalizeMeanDev MeanDevInitial = Nothing
finalizeMeanDev (MeanDevAcc _ Nothing _) = Nothing
finalizeMeanDev (MeanDevAcc mn (Just var) _) = Just (mn, fromVariance var)

-- FIXME: median
updateNumericState :: Real a
                   => NumericState -> a -> NumericState
updateNumericState acc x =
    (stateMinimum %~ (flip updateMinimum x))
  . (stateMaximum %~ (flip updateMaximum x))
  . (stateMeanDev %~ (flip updateMeanDev x))
  $!! acc
{-# INLINE updateNumericState #-}
