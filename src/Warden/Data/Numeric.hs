{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Data.Numeric (
    FieldNumericState(..)

  , KAcc(..)

  , Maximum(..)
  , renderMaximum

  , Mean(..)
  , renderMean

  , MeanAcc(..)

  , MeanDevAcc(..)
  , finalizeMeanDev

  , Median(..)
  , renderMedian

  , MNumericField(..)

  , MStdDevAcc(..)

  , Minimum(..)
  , renderMinimum

  , NumericField(..)

  , NumericState(..)
  , initialNumericState
  , stateMaximum
  , stateMeanDev
  , stateMinimum

  , NumericFieldSummary(..)

  , NumericSummary(..)

  , StdDev(..)
  , mkStdDev
  , renderStdDev

  , StdDevAcc(..)
  , finalizeStdDevAcc

  , Variance(..)
  , stdDevAccFromVariance
  , varianceFromStdDevAcc
  ) where

import           Control.DeepSeq.Generics (genericRnf)
import           Control.Lens (makeLenses)

import           Data.AEq (AEq, (===), (~==))
import qualified Data.Vector as V

import           GHC.Generics (Generic)

import           P

import           Warden.Data.Sampling (Sample(..))

data Minimum =
    Minimum !Double
  | NoMinimum
  deriving (Eq, Show, Generic)

instance NFData Minimum where rnf = genericRnf

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

renderMinimum :: Text -> Minimum -> Text
renderMinimum missing NoMinimum = missing
renderMinimum _ (Minimum x) = renderFractional x

data Maximum =
    Maximum !Double
  | NoMaximum
  deriving (Eq, Show, Generic)

instance NFData Maximum where rnf = genericRnf

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

renderMaximum :: Text -> Maximum -> Text
renderMaximum missing NoMaximum = missing
renderMaximum _ (Maximum x) = renderFractional x

-- | Counter param for mean/stddev calculation. Equal to one plus the number
-- of records seen.
newtype KAcc =
  KAcc {
    getKAcc :: Int
  } deriving (Eq, Show, Generic, Num)

instance NFData KAcc where rnf = genericRnf

-- | Preliminary mean, still accumulating.
newtype MeanAcc =
  MeanAcc {
    unMeanAcc :: Double
  } deriving (
      Eq
    , Show
    , Generic
    , Ord
    , Num
    , Real
    , Fractional
    , RealFrac
    , Floating
    , RealFloat
  )

instance NFData MeanAcc where rnf = genericRnf

instance AEq MeanAcc where
  (===) = (==)

  (MeanAcc x) ~== (MeanAcc y) = x ~== y

-- | Final mean.
data Mean =
    NoMean
  | Mean !Double
  deriving (Eq, Show, Generic)

instance NFData Mean where rnf = genericRnf

instance AEq Mean where
  NoMean === NoMean = True
  NoMean === _ = False
  _ === NoMean = False
  (Mean x) === (Mean y) = x === y

  NoMean ~== NoMean = True
  NoMean ~== _ = False
  _ ~== NoMean = False
  (Mean x) ~== (Mean y) = x ~== y

renderMean :: Text -> Mean -> Text
renderMean missing NoMean = missing
renderMean _ (Mean x) = renderFractional x

data Median =
    Median !Double
  | NoMedian
  deriving (Eq, Show, Generic)

instance NFData Median where rnf = genericRnf

renderMedian :: Text -> Median -> Text
renderMedian missing NoMedian = missing
renderMedian _ (Median x) = renderFractional x

-- | Accumulator for standard deviation calculation. Closer to variance than 
-- standard deviation to avoid repeated square roots.
--
-- \( acc = \sigma^{2} (k - 1) \)
--
-- Where `acc` is 'StdDevAcc' and `k` is the 'KAcc'.
newtype StdDevAcc =
  StdDevAcc {
    unStdDevAcc :: Double
  } deriving (
      Eq
    , Show
    , Generic
    , Ord
    , Num
    , Real
    , Fractional
    , RealFrac
    , Floating
    , RealFloat
  )

instance NFData StdDevAcc where rnf = genericRnf

instance AEq StdDevAcc where
  (===) = (==)

  (StdDevAcc x) ~== (StdDevAcc y) = x ~== y

-- | Possibly-uninitialized 'StdDevAcc'.
data MStdDevAcc =
    NoStdDevAcc
  | MStdDevAcc !StdDevAcc
  deriving (Eq, Show, Generic)

instance NFData MStdDevAcc where rnf = genericRnf

instance AEq MStdDevAcc where
  (===) = (==)

  NoStdDevAcc ~== NoStdDevAcc = True
  NoStdDevAcc ~== _ = False
  _ ~== NoStdDevAcc = False
  (MStdDevAcc sda1) ~== (MStdDevAcc sda2) = sda1 ~== sda2

newtype Variance =
  Variance {
    unVariance :: Double
  } deriving (Eq, Show, Generic)

instance NFData Variance where rnf = genericRnf

data StdDev =
    NoStdDev
  | StdDev !Double
  deriving (Eq, Show, Generic)

instance NFData StdDev where rnf = genericRnf

instance AEq StdDev where
  NoStdDev === NoStdDev = True
  NoStdDev === _ = False
  _ === NoStdDev = False
  (StdDev x) === (StdDev y) = x === y

  NoStdDev ~== NoStdDev = True
  NoStdDev ~== _ = False
  _ ~== NoStdDev = False
  (StdDev x) ~== (StdDev y) = x ~== y

renderStdDev :: Text -> StdDev -> Text
renderStdDev missing NoStdDev = missing
renderStdDev _ (StdDev x) = renderFractional x

mkStdDev :: Double -> StdDev
mkStdDev v
  | v < 0.0   = NoStdDev
  | otherwise = StdDev v

-- | So we can cheaply keep track of long-term change in numeric datasets.
--   Will probably also end up in brandix.
data NumericSummary =
    NoNumericSummary
  | NumericSummary
      !Minimum
      !Maximum
      !Mean
      !StdDev
      !Median 
      !Sample
  deriving (Eq, Show, Generic)

instance NFData NumericSummary where rnf = genericRnf

data FieldNumericState =
    FieldNumericState !(V.Vector NumericState)
  | NoFieldNumericState
  deriving (Eq, Show, Generic)

instance NFData FieldNumericState where rnf = genericRnf

data NumericFieldSummary =
    NumericFieldSummary !(V.Vector NumericSummary)
  | NoNumericFieldSummary
  deriving (Eq, Show, Generic)

instance NFData NumericFieldSummary where rnf = genericRnf

data MeanDevAcc =
    MeanDevInitial
  | MeanDevAcc !MeanAcc !MStdDevAcc !KAcc
  deriving (Eq, Show, Generic)

instance NFData MeanDevAcc where rnf = genericRnf

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

-- | State stored for every field with numeric
-- observations.
data NumericState =
  NumericState {
      _stateMinimum :: !Minimum
    , _stateMaximum :: !Maximum
    -- | Accumulator for mean/stddev - Welford's method
    -- stores the current mean and the current
    -- sum-of-squares of deviations from the mean.
    , _stateMeanDev :: !MeanDevAcc
    } deriving (Eq, Show, Generic)

instance NFData NumericState where rnf = genericRnf

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

data MNumericField =
    NoNumericField
  | MNumericField !NumericField
  deriving (Eq, Show)

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

finalizeStdDevAcc :: KAcc -> StdDevAcc -> StdDev
finalizeStdDevAcc ka sda =
  stdDevFromVariance $ varianceFromStdDevAcc ka sda
{-# INLINE finalizeStdDevAcc #-}

finalizeMeanDev :: MeanDevAcc -> (Mean, StdDev)
finalizeMeanDev MeanDevInitial = (NoMean, NoStdDev)
finalizeMeanDev (MeanDevAcc _ NoStdDevAcc _) = (NoMean, NoStdDev)
finalizeMeanDev (MeanDevAcc mn (MStdDevAcc sda) n) = (Mean (unMeanAcc mn), finalizeStdDevAcc n sda)
{-# INLINE finalizeMeanDev #-}
