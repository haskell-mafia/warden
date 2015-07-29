{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Data.Numeric (
    Minimum(..)
  , Maximum(..)
  , Mean(..)
  , Median(..)
  , Variance(..)
  , NumericSummary(..)
  , accumMinimum
  , accumMaximum
  , accumMean
  ) where

import P

newtype Minimum = Minimum { getMininum :: Double }
  deriving (Eq, Show, Ord)

newtype Maximum = Maximum { getMaximum :: Double }
  deriving (Eq, Show, Ord)

newtype Mean = Mean { getMean :: Double }
  deriving (Eq, Show, Ord)

newtype Median = Median { getMedian :: Double }
  deriving (Eq, Show)

newtype Variance = Variance { getVariance :: Double }
  deriving (Eq, Show)

-- | So we can cheaply keep track of long-term change in numeric datasets.
--   Will probably also end up in brandix.
data NumericSummary = NumericSummary Minimum
                                     Maximum
                                     Mean
                                     (Maybe Variance)
                                     (Maybe Median)
  deriving (Eq, Show)

accumCompare :: (a -> a -> Bool)
             -> a -> a -> a
accumCompare cmp cur prev
  | cmp cur prev = cur
  | otherwise    = prev

accumMinimum :: Real a
             => Minimum -> a -> Minimum
accumMinimum (Minimum acc) x =
  let x' = (fromRational . toRational) x in
  Minimum $ accumCompare (<) acc x'

accumMaximum :: Real a
             => Maximum -> a -> Maximum
accumMaximum (Maximum acc) x =
  let x' = (fromRational . toRational) x in
  Maximum $ accumCompare (>) acc x'

accumMean :: Real a
          => Int
          -> Mean -> a -> Mean
accumMean n (Mean acc) x =
  let x' = (fromRational . toRational) x in
  Mean $ acc + x' / (fromIntegral n)
