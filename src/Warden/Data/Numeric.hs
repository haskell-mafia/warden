{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.Data.Numeric (
    KAcc(..)
  , Maximum(..)
  , Mean(..)
  , MeanAcc(..)
  , MeanDevAcc(..)
  , Median(..)
  , Minimum(..)
  , NumericField(..)
  , NumericState(..)
  , NumericSummary(..)
  , StdDev(..)
  , StdDevAcc(..)
  , Variance(..)
  , initialNumericState
  , mkStdDev
  , stateMaximum
  , stateMeanDev
  , stateMinimum
  ) where

import           Control.Lens (makeLenses)

import           Data.AEq (AEq, (===), (~==))

import           GHC.Generics (Generic)

import           P

data Minimum =
    Minimum {-# UNPACK #-} !Double
  | NoMinimum
  deriving (Eq, Show, Generic)

instance NFData Minimum

instance AEq Minimum where
  (===) = (==)
  (~==) = (==)

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

instance AEq Maximum where
  (===) = (==)
  (~==) = (==)

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

-- | Counter param for mean/stddev calculation. Equal to one plus the number
-- of records seen.
newtype KAcc =
  KAcc {
    getKAcc :: Int
  } deriving (Eq, Show, Generic, Num)

instance NFData KAcc

-- | Preliminary mean, still accumulating.
newtype MeanAcc =
  MeanAcc {
    unMeanAcc :: Double
  } deriving (Eq, Show, Generic)

instance NFData MeanAcc

instance AEq MeanAcc where
  (===) = (==)

  (MeanAcc x) ~== (MeanAcc y) = x ~== y

-- | Final mean.
data Mean =
    NoMean
  | Mean {-# UNPACK #-} !Double
  deriving (Eq, Show, Generic)

instance NFData Mean

instance AEq Mean where
  NoMean === NoMean = True
  NoMean === _ = False
  _ === NoMean = False
  (Mean x) === (Mean y) = x === y

  NoMean ~== NoMean = True
  NoMean ~== _ = False
  _ ~== NoMean = False
  (Mean x) ~== (Mean y) = x ~== y

data Median =
    Median {-# UNPACK #-} !Double
  | NoMedian
  deriving (Eq, Show, Generic)

instance NFData Median

-- | Accumulator for standard deviation calculation. Closer to variance than 
-- standard deviation to avoid repeated square roots.
--
-- \( acc = \sigma^{2} (k - 1) \)
--
-- Where `acc` is 'StdDevAcc' and `k` is the 'KAcc'.
newtype StdDevAcc =
  StdDevAcc {
    unStdDevAcc :: Double
  } deriving (Eq, Show, Generic)

instance NFData StdDevAcc

instance AEq StdDevAcc where
  (===) = (==)

  (StdDevAcc x) ~== (StdDevAcc y) = x ~== y

newtype Variance =
  Variance {
    unVariance :: Double
  } deriving (Eq, Show, Generic)

instance NFData Variance

data StdDev =
    NoStdDev
  | StdDev {-# UNPACK #-} !Double
  deriving (Eq, Show, Generic)

instance NFData StdDev

instance AEq StdDev where
  NoStdDev === NoStdDev = True
  NoStdDev === _ = False
  _ === NoStdDev = False
  (StdDev x) === (StdDev y) = x === y

  NoStdDev ~== NoStdDev = True
  NoStdDev ~== _ = False
  _ ~== NoStdDev = False
  (StdDev x) ~== (StdDev y) = x ~== y

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
  | MeanDevAcc {-# UNPACK #-} !MeanAcc !(Maybe StdDevAcc) {-# UNPACK #-} !KAcc
  deriving (Eq, Show, Generic)

instance NFData MeanDevAcc

instance AEq MeanDevAcc where
  (===) = (==)

  MeanDevInitial ~== MeanDevInitial = True
  MeanDevInitial ~== _ = False
  _ ~== MeanDevInitial = False
  (MeanDevAcc mu1 s21 n1) ~== (MeanDevAcc mu2 s22 n2) = and [
      mu1 ~== mu2
    , s21 ~== s22
    , n1 == n2
    ]

data NumericState =
  NumericState {
      _stateMinimum :: !Minimum
    , _stateMaximum :: !Maximum
    , _stateMeanDev :: !MeanDevAcc
    } deriving (Eq, Show, Generic)

instance NFData NumericState

makeLenses ''NumericState

instance AEq NumericState where
  (===) = (==)

  (NumericState mn1 mx1 mda1) ~== (NumericState mn2 mx2 mda2) = and [
      mn1 ~== mn2
    , mx1 ~== mx2
    , mda1 ~== mda2
    ]

initialNumericState :: NumericState
initialNumericState =
  NumericState
    NoMinimum
    NoMaximum
    MeanDevInitial

-- | For numeric purposes we treat all numbers as reals.
newtype NumericField =
  NumericField {
    unNumericField :: Double
  } deriving (Eq, Show)
