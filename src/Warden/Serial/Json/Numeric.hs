{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Serial.Json.Numeric (
    toNumericSummary,
    fromNumericSummary
) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T

import           P

import           Warden.Data.Numeric

fromNumericSummary :: NumericSummary -> Value
fromNumericSummary (NumericSummary mn mx mean s md) = object [
    "version"  .= ("v1" :: Text)
  , "minimum"  .= fromMinimum mn
  , "maximum"  .= fromMaximum mx
  , "mean"     .= fromMean mean
  , "stddev"   .= fromStdDev s
  , "median"   .= fromMedian md
  ]

toNumericSummary :: Value -> Parser NumericSummary
toNumericSummary (Object o) =
  o .: "version" >>= \case
    "v1" -> NumericSummary
      <$> (toMinimum =<< (o .: "minimum"))
      <*> (toMaximum =<< (o .: "maximum"))
      <*> (toMean =<< (o .: "mean"))
      <*> (toStdDev =<< (o .: "stddev"))
      <*> (toMedian =<< (o .: "median"))
    v -> fail $ "Warden.Data.Numeric.NumericSummary: unknown version [" <> v <> "]"
toNumericSummary x = typeMismatch "Warden.Data.Numeric.NumericSummary" x

fromMaximum :: Maximum -> Value
fromMaximum (Maximum v) = object [
    "type" .= String "maximum"
  , "value" .= toJSON v
  ]
fromMaximum NoMaximum = object [
    "type" .= String "no-maximum"
  ]

toMaximum :: Value -> Parser Maximum
toMaximum (Object o) = do
  o .: "type" >>= \case
    "no-maximum" -> pure NoMaximum
    "maximum" -> do
      v <- parseJSON =<< (o .: "value")
      pure $ Maximum v
    s -> fail . T.unpack $ "invalid Maximum type: " <> s
toMaximum x = typeMismatch "Warden.Data.Numeric.Maximum" x

fromMinimum :: Minimum -> Value
fromMinimum (Minimum v) = object [
    "type" .= String "minimum"
  , "value" .= toJSON v
  ]
fromMinimum NoMinimum = object [
    "type" .= String "no-minimum"
  ]

toMinimum :: Value -> Parser Minimum
toMinimum (Object o) = do
  o .: "type" >>= \case
    "no-minimum" -> pure NoMinimum
    "minimum" -> do
      v <- parseJSON =<< (o .: "value")
      pure $ Minimum v
    s -> fail . T.unpack $ "invalid Minimum type: " <> s
toMinimum x = typeMismatch "Warden.Data.Numeric.Minimum" x

fromMedian :: Median -> Value
fromMedian (Median v) = object [
    "type" .= String "median"
  , "value" .= toJSON v
  ]
fromMedian NoMedian = object [
    "type" .= String "no-median"
  ]

toMedian :: Value -> Parser Median
toMedian (Object o) = do
  o .: "type" >>= \case
    "no-median" -> pure NoMedian
    "median" -> do
      v <- parseJSON =<< (o .: "value")
      pure $ Median v
    s -> fail . T.unpack $ "invalid Median type: " <> s
toMedian x = typeMismatch "Warden.Data.Numeric.Median" x

fromMean :: Mean -> Value
fromMean (Mean v) = object [
    "type" .= String "mean"
  , "value" .= toJSON v
  ]
fromMean NoMean = object [
    "type" .= String "no-mean"
  ]

toMean :: Value -> Parser Mean
toMean (Object o) = do
  o .: "type" >>= \case
    "no-mean" -> pure NoMean
    "mean" -> do
      v <- parseJSON =<< (o .: "value")
      pure $ Mean v
    s -> fail . T.unpack $ "invalid Mean type: " <> s
toMean x = typeMismatch "Warden.Data.Numeric.Mean" x

fromStdDev :: StdDev -> Value
fromStdDev (StdDev v) = object [
    "type" .= String "stddev"
  , "value" .= toJSON v
  ]
fromStdDev NoStdDev = object [
    "type" .= String "no-stddev"
  ]

toStdDev :: Value -> Parser StdDev
toStdDev (Object o) = do
  o .: "type" >>= \case
    "no-stddev" -> pure NoStdDev
    "stddev" -> do
      v <- parseJSON =<< (o .: "value")
      pure $ StdDev v
    s -> fail . T.unpack $ "invalid StdDev type: " <> s
toStdDev x = typeMismatch "Warden.Data.Numeric.StdDev" x
