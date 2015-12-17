module Warden.Numeric (
    MeanAcc(..)
  , updateMinimum
  , updateMaximum
  , updateMean
  , finalizeMean
  ) where

import           P

import           Warden.Data

data MeanAcc =
    MeanInitial
  | MeanAcc Mean Count
  deriving (Eq, Show)

updateMinimum :: Real a
              => Minimum -> a -> Minimum
updateMinimum acc x =
  let x' = (Minimum . Just . fromRational . toRational) x
  in acc <> x'

updateMaximum :: Real a
              => Maximum -> a -> Maximum
updateMaximum acc x =
  let x' = (Maximum . Just . fromRational . toRational) x
  in acc <> x'

-- Numerically-stable mean:
--
-- \( \frac{1}{n} \sum_{x \in X} x \equiv M_1 = X_1, M_k = M_{k-1} + \frac{(X_k - M_{k-1})}{k} \)
updateMean :: Real a
           => MeanAcc -> a -> MeanAcc
updateMean macc x =
  let x' = (fromRational . toRational) x in case macc of
  MeanInitial ->
    let i   = Count 1
        acc = Mean 0 in
    update' acc i x'
  (MeanAcc acc i) ->
    update' acc i x'
  where
    update' (Mean acc) (Count i) v =
      let delta = v - acc in
      MeanAcc (Mean $ acc + delta / (fromIntegral i)) . Count $ i + 1

finalizeMean :: MeanAcc -> Maybe Mean
finalizeMean MeanInitial = Nothing
finalizeMean (MeanAcc mn _) = Just mn
