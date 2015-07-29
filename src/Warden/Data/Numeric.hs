{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}

module Warden.Data.Numeric (
    Minimum(..)
  , Maximum(..)
  , Mean(..)
  , Median(..)
  , Variance(..)
  , mkVariance
  , NumericSummary(..)
  , accumMinimum
  , accumMaximum
  , accumMean
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text        (Text)
import           P

newtype Minimum = Minimum { getMininum :: Double }
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

newtype Maximum = Maximum { getMaximum :: Double }
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

newtype Mean = Mean { getMean :: Double }
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

newtype Median = Median { getMedian :: Double }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Variance = Variance { getVariance :: Double }
  deriving (Eq, Show, ToJSON)

mkVariance :: Double -> Maybe Variance
mkVariance v
  | v < 0.0   = Nothing
  | otherwise = Just (Variance v)

instance FromJSON Variance where
  parseJSON (Number v) = case mkVariance ((fromRational . toRational) v) of
    Nothing -> fail "Variance must not be negative"
    Just v' -> pure v'
  parseJSON Null       = pure $ Variance Nothing
  parseJSON x          = typeMismatch "Variance" x

-- | So we can cheaply keep track of long-term change in numeric datasets.
--   Will probably also end up in brandix.
data NumericSummary = NumericSummary Minimum
                                     Maximum
                                     Mean
                                     (Maybe Variance)
                                     (Maybe Median)
  deriving (Eq, Show)

instance ToJSON NumericSummary where
  toJSON (NumericSummary mn mx mean v md) = object [
      "version"  .= ("v1" :: Text)
    , "minimum"  .= mn
    , "maximum"  .= mx
    , "mean"     .= mean
    , "variance" .= v
    , "median"   .= md
    ]

instance FromJSON NumericSummary where
  parseJSON (Object o) =
    o .: "version" >>= \case
      "v1" -> NumericSummary
                <$> o .: "minimum"
                <*> o .: "maximum"
                <*> o .: "mean"
                <*> o .: "variance"
                <*> o .: "median"
      v    -> fail $ "NumericSummary: unknown version [" <> v <> "]"
  parseJSON x          = typeMismatch "NumericSummary" x

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
