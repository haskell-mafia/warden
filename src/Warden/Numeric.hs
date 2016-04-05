{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Numeric (
    combineMeanAcc
  , combineMeanDevAcc
  , combineNumericState
  , combineStdDevAcc
  , finalizeMeanDev
  , finalizeStdDevAcc
  , stdDevAccFromVariance
  , summarizeNumericState
  , updateMinimum
  , updateMaximum
  , updateMeanDev
  , updateNumericState
  , varianceFromStdDevAcc
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

-- | Minimal-error mean and standard deviation.
--
-- From Knuth (TAoCP v2, Seminumerical Algorithms, p232).
--
-- \( \frac{1}{n} \sum_{x \in X} x \equiv M_1 = X_1, M_k = M_{k-1} + \frac{(X_k - M_{k-1})}{k} \)
updateMeanDev :: Real a
              => MeanDevAcc -> a -> MeanDevAcc
updateMeanDev !macc x =
  let x' = (fromRational . toRational) x in case macc of
  MeanDevInitial ->
    let i = KAcc 1
        m = MeanAcc 0
        s = Nothing
    in update' m s i x'
  (MeanDevAcc m s i) ->
    update' m s i x'
  where
    update' (MeanAcc m) s (KAcc i) v =
      let delta = v - m
          m'    = MeanAcc $ m + delta / (fromIntegral i)
          i'    = KAcc $ i + 1
          s'    = case s of
                    Nothing ->
                      Just $ StdDevAcc 0
                    Just (StdDevAcc sda) ->
                      Just . StdDevAcc $ sda + (delta * (v - (unMeanAcc m')))
      in MeanDevAcc m' s' i'
{-# INLINE updateMeanDev #-}

finalizeMeanDev :: MeanDevAcc -> (Mean, StdDev)
finalizeMeanDev MeanDevInitial = (NoMean, NoStdDev)
finalizeMeanDev (MeanDevAcc _ Nothing _) = (NoMean, NoStdDev)
finalizeMeanDev (MeanDevAcc mn (Just sda) n) = (Mean (unMeanAcc mn), finalizeStdDevAcc n sda)

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
{-# INLINE combineMeanDevAcc #-}

-- | Combine stddev accumulators of two subsets by converting to variance
-- (pretty cheap), combining the variances (less cheap), and converting back.
--
-- There's almost certainly a better way to do this.
combineStdDevAcc :: MeanAcc -- ^ Combined mean.
                -> (MeanAcc, Maybe StdDevAcc, KAcc) -- ^ First subset.
                -> (MeanAcc, Maybe StdDevAcc, KAcc) -- ^ Second subset.
                -> Maybe StdDevAcc
combineStdDevAcc _ (_, Nothing, _) (_, Nothing, _) =
  Nothing
combineStdDevAcc _ (_, Just (StdDevAcc s1), _) (_, Nothing, _) =
  Just $ StdDevAcc s1
combineStdDevAcc _ (_, Nothing, _) (_, Just (StdDevAcc s2), _) =
  Just $ StdDevAcc s2
combineStdDevAcc muHat (mu1, Just sda1, c1) (mu2, Just sda2, c2) =
  let var1 = varianceFromStdDevAcc c1 sda1
      var2 = varianceFromStdDevAcc c2 sda2 in
  Just . stdDevAccFromVariance (c1 + c2 - (KAcc 1)) $
    combineVariance muHat (mu1, var1, c1) (mu2, var2, c2)
{-# INLINE combineStdDevAcc #-}

-- | Combine variances of two subsets of a sample (that is, exact variance of
-- datasets rather than estimate of variance of population).
combineVariance :: MeanAcc -- ^ Combined mean.
                -> (MeanAcc, Variance, KAcc) -- ^ First subset.
                -> (MeanAcc, Variance, KAcc) -- ^ Second subset.
                -> Variance
combineVariance (MeanAcc muHat) (MeanAcc mu1, Variance var1, KAcc c1) (MeanAcc mu2, Variance var2, KAcc c2) =
  let t1 = c1' * (var1 + (mu1 ** two))
      t2 = c2' * (var2 + (mu2 ** two)) in
  Variance $ ((t1 + t2) / (c1' + c2')) - (muHat ** two)
  where
    c1' = fromIntegral $ c1 - 1

    c2' = fromIntegral $ c2 - 1

    two = 2.0 :: Double
{-# INLINE combineVariance #-}

-- | Combine mean of two subsets, given subset means and size.
combineMeanAcc :: (MeanAcc, KAcc) -> (MeanAcc, KAcc) -> MeanAcc
combineMeanAcc (MeanAcc mu1, KAcc c1) (MeanAcc mu2, KAcc c2) =
  let c1' = fromIntegral $ c1 - 1
      c2' = fromIntegral $ c2 - 1 in
  MeanAcc $ ((mu1 * c1') + (mu2 * c2')) / (c1' + c2')
{-# INLINE combineMeanAcc #-}

finalizeStdDevAcc :: KAcc -> StdDevAcc -> StdDev
finalizeStdDevAcc ka sda =
  stdDevFromVariance $ varianceFromStdDevAcc ka sda
{-# INLINE finalizeStdDevAcc #-}

varianceFromStdDevAcc :: KAcc -> StdDevAcc -> Variance
varianceFromStdDevAcc (KAcc n) (StdDevAcc sda) =
  Variance $ sda / fromIntegral (n - 1)
{-# INLINE varianceFromStdDevAcc #-}

stdDevAccFromVariance :: KAcc -> Variance -> StdDevAcc
stdDevAccFromVariance (KAcc n) (Variance var) =
  StdDevAcc $ var * fromIntegral (n - 1)
{-# INLINE stdDevAccFromVariance #-}

stdDevFromVariance :: Variance -> StdDev
stdDevFromVariance = StdDev . sqrt . unVariance
{-# INLINE stdDevFromVariance #-}

-- FIXME: not associative
combineNumericState :: NumericState -> NumericState -> NumericState
combineNumericState ns1 ns2 =
    (stateMinimum %~ (<> (ns1 ^. stateMinimum)))
  . (stateMaximum %~ (<> (ns1 ^. stateMaximum)))
  . (stateMeanDev %~ (combineMeanDevAcc (ns1 ^. stateMeanDev)))
  $!! ns2
{-# INLINE combineNumericState #-}
