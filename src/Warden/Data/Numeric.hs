{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Warden.Data.Numeric (
    Minimum(..)
  , Maximum(..)
  , Mean(..)
  , Median(..)
  , Variance(..)
  , mkVariance
  , NumericSummary(..)
  , updateMinimum
  , updateMaximum
  , updateMean
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text        (Text)
import           P

-- | Semigroup <cmp, a> => Monoid <mcompare, Maybe a>
mcompare :: (a -> a -> Bool)
         -> Maybe a -> Maybe a -> Maybe a
mcompare _ Nothing first       = first
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

newtype Mean = Mean { getMean :: Maybe Double }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Median = Median { getMedian :: Maybe Double }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Variance = Variance { getVariance :: Maybe Double }
  deriving (Eq, Show, ToJSON)

mkVariance :: Double -> Maybe Variance
mkVariance v
  | v < 0.0   = Nothing
  | otherwise = Just (Variance (Just v))

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
                                     Variance
                                     Median
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

-- | This fold requires `n` to be known; quotient-of-sums could be used for
--   small datasets (or small values), but in the general case is prone to
--   overflow.
updateMean :: Real a
           => Int
           -> Mean -> a -> Mean
updateMean n (Mean Nothing) x =
  let x' = (fromRational . toRational) x in
  Mean . Just $ x' / (fromIntegral n)
updateMean n (Mean (Just acc)) x =
  let x' = (fromRational . toRational) x in
  Mean . Just $ acc + x' / (fromIntegral n)
