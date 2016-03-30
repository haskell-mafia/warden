 {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Numeric (
    combineMeanAcc
  , combineMeanDevAcc
  , combineVariance
  , finalizeMeanDev
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

-- FIXME: this is unstable, think of something less dumb
combineMeanDevAcc :: MeanDevAcc -> MeanDevAcc -> MeanDevAcc
combineMeanDevAcc MeanDevInitial MeanDevInitial = MeanDevInitial
combineMeanDevAcc MeanDevInitial md2 = md2
combineMeanDevAcc md1 MeanDevInitial = md1
combineMeanDevAcc (MeanDevAcc mu1 s1 c1) (MeanDevAcc mu2 s2 c2) =
  let muHat = combineMeanAcc (mu1, c1) (mu2, c2)
      sHat = combineVariance muHat (mu1, s1, c1) (mu2, s2, c2) in
  MeanDevAcc muHat sHat (c1 + c2)
{-# INLINE combineMeanDevAcc #-}

-- | Combine variance of two subsets.
combineVariance :: MeanAcc -- ^ Combined mean.
                -> (MeanAcc, Maybe Variance, Count) -- ^ First sample.
                -> (MeanAcc, Maybe Variance, Count) -- ^ Second sample.
                -> Maybe Variance
combineVariance _ (_, Nothing, _) (_, Nothing, _) =
  Nothing
combineVariance _ (_, Just (Variance s1), _) (_, Nothing, _) =
  Just $ Variance s1
combineVariance _ (_, Nothing, _) (_, Just (Variance s2), _) =
  Just $ Variance s2
combineVariance (MeanAcc muHat) (MeanAcc mu1, Just (Variance s1), Count c1) (MeanAcc mu2, Just (Variance s2), Count c2) =
  let t1 = c1' * (s1 + (mu1 ** two))
      t2 = c2' * (s2 + (mu2 ** two)) in
  Just . Variance $ ((t1 + t2) / (c1' + c2')) - (muHat ** two)
  where
    c1' = fromIntegral c1

    c2' = fromIntegral c2

    two = 2.0 :: Double
{-# INLINE combineVariance #-}

-- | Combine mean of two subsets, given subset means and size.
combineMeanAcc :: (MeanAcc, Count) -> (MeanAcc, Count) -> MeanAcc
combineMeanAcc (MeanAcc mu1, Count c1) (MeanAcc mu2, Count c2) =
  let c1' = fromIntegral c1
      c2' = fromIntegral c2 in
  MeanAcc $ ((mu1 * c1') + (mu2 * c2')) / (c1' + c2')
{-# INLINE combineMeanAcc #-}
