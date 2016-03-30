{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.Data.Numeric (
    Count(..)
  , Maximum(..)
  , Mean(..)
  , MeanAcc(..)
  , MeanDevAcc(..)
  , Median(..)
  , Minimum(..)
  , NumericState(..)
  , NumericSummary(..)
  , StdDev(..)
  , Variance(..)
  , fromVariance
  , initialNumericState
  , mkStdDev
  , stateMaximum
  , stateMeanDev
  , stateMinimum
  ) where

import           Control.Lens (makeLenses)

import           Data.Aeson (ToJSON, FromJSON)

import           GHC.Generics (Generic)

import           P

data Minimum =
    Minimum {-# UNPACK #-} !Double
  | NoMinimum
  deriving (Eq, Show, Generic)

instance NFData Minimum

instance Monoid Minimum where
  mempty  = NoMinimum
  mappend x y = mcompare x y
    where
      mcompare NoMinimum init = init
      mcompare (Minimum prev) NoMinimum = Minimum prev
      mcompare (Minimum prev) (Minimum cur) =
        if cur < prev
          then Minimum cur
          else Minimum prev
  {-# INLINE mappend #-}

data Maximum =
    Maximum {-# UNPACK #-} !Double
  | NoMaximum
  deriving (Eq, Show, Generic)

instance NFData Maximum

instance Monoid Maximum where
  mempty  = NoMaximum
  mappend x y = mcompare x y
    where
      mcompare NoMaximum init = init
      mcompare (Maximum prev) NoMaximum = Maximum prev
      mcompare (Maximum prev) (Maximum cur) =
        if cur > prev
          then Maximum cur
          else Maximum prev
  {-# INLINE mappend #-}

newtype Count =
  Count {
    getCount :: Int
  } deriving (Eq, Show, ToJSON, FromJSON, Generic, Num)

instance NFData Count

-- | Preliminary mean, still accumulating.
newtype MeanAcc =
  MeanAcc {
    unMeanAcc :: Double
  } deriving (Eq, Show, ToJSON, FromJSON, Generic)

instance NFData MeanAcc

-- | Final mean.
data Mean =
    NoMean
  | Mean {-# UNPACK #-} !Double
  deriving (Eq, Show, Generic)

instance NFData Mean

data Median =
    Median {-# UNPACK #-} !Double
  | NoMedian
  deriving (Eq, Show, Generic)

instance NFData Median

newtype Variance = Variance { getVariance :: Double }
  deriving (Eq, Show, ToJSON, FromJSON, Generic)

instance NFData Variance

fromVariance :: Variance -> StdDev
fromVariance = StdDev . sqrt . getVariance

data StdDev =
    NoStdDev
  | StdDev {-# UNPACK #-} !Double
  deriving (Eq, Show, Generic)

instance NFData StdDev

mkStdDev :: Double -> StdDev
mkStdDev v
  | v < 0.0   = NoStdDev
  | otherwise = StdDev v

-- | So we can cheaply keep track of long-term change in numeric datasets.
--   Will probably also end up in brandix.
data NumericSummary = NumericSummary !Minimum
                                     !Maximum
                                     !Mean
                                     !StdDev
                                     !Median
  deriving (Eq, Show, Generic)

instance NFData NumericSummary

data MeanDevAcc =
    MeanDevInitial
  | MeanDevAcc {-# UNPACK #-} !MeanAcc !(Maybe Variance) {-# UNPACK #-} !Count
  deriving (Eq, Show, Generic)

instance NFData MeanDevAcc

data NumericState =
  NumericState {
      _stateMinimum :: !Minimum
    , _stateMaximum :: !Maximum
    , _stateMeanDev :: !MeanDevAcc
    } deriving (Eq, Show, Generic)

instance NFData NumericState

makeLenses ''NumericState

initialNumericState :: NumericState
initialNumericState =
  NumericState
    NoMinimum
    NoMaximum
    MeanDevInitial
