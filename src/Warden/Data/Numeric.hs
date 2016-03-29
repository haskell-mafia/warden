{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.Numeric (
    Minimum(..)
  , Maximum(..)
  , Mean(..)
  , Count(..)
  , Median(..)
  , StdDev(..)
  , NumericSummary(..)
  , Variance(..)
  , fromVariance
  , mkStdDev
  ) where

import           Data.Aeson
import           Data.Aeson.Types

import           P

data Minimum =
    Minimum {-# UNPACK #-} !Double
  | NoMinimum
  deriving (Eq, Show)

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

data Maximum =
    Maximum {-# UNPACK #-} !Double
  | NoMaximum
  deriving (Eq, Show)

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

newtype Count = Count { getCount :: Int }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Mean = Mean { getMean :: Double }
  deriving (Eq, Show, ToJSON, FromJSON)

data Median =
    Median {-# UNPACK #-} !Double
  | NoMedian
  deriving (Eq, Show)

newtype Variance = Variance { getVariance :: Double }
  deriving (Eq, Show, ToJSON, FromJSON)

fromVariance :: Variance -> StdDev
fromVariance = StdDev . sqrt . getVariance

newtype StdDev = StdDev { getStdDev :: Double }
  deriving (Eq, Show, ToJSON)

mkStdDev :: Double -> Maybe StdDev
mkStdDev v
  | v < 0.0   = Nothing
  | otherwise = Just $ StdDev v

instance FromJSON StdDev where
  parseJSON (Number v) = case mkStdDev ((fromRational . toRational) v) of
    Nothing -> fail "StdDev must not be negative"
    Just v' -> pure v'
  parseJSON x          = typeMismatch "StdDev" x

-- | So we can cheaply keep track of long-term change in numeric datasets.
--   Will probably also end up in brandix.
data NumericSummary = NumericSummary !Minimum
                                     !Maximum
                                     {-# UNPACK #-} !Mean
                                     {-# UNPACK #-} !StdDev
                                     !Median
  deriving (Eq, Show)
