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

-- | Semigroup <cmp, a> => Monoid <mcompare, Maybe a>
mcompare :: (a -> a -> Bool)
         -> Maybe a -> Maybe a -> Maybe a
mcompare _ Nothing init        = init
mcompare _ (Just prev) Nothing = Just prev
mcompare cmp (Just prev) (Just cur)
  | cmp cur prev               = Just cur
  | otherwise                  = Just prev

newtype Minimum = Minimum { getMinimum :: Maybe Double }
  deriving (Eq, Show, ToJSON, FromJSON)

instance Monoid Minimum where
  mempty  = Minimum Nothing
  mappend (Minimum x) (Minimum y) = Minimum $ mcompare (<) x y

newtype Maximum = Maximum { getMaximum :: Maybe Double }
  deriving (Eq, Show, ToJSON, FromJSON)

instance Monoid Maximum where
  mempty  = Maximum Nothing
  mappend (Maximum x) (Maximum y) = Maximum $ mcompare (>) x y

newtype Count = Count { getCount :: Int }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Mean = Mean { getMean :: Double }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Median = Median { getMedian :: Maybe Double }
  deriving (Eq, Show, ToJSON, FromJSON)

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
