{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Serial.Json.Numeric (
    fromNumericFieldSummary
  , toNumericFieldSummary
  , fromNumericSummary
  , toNumericSummary
) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V

import           P

import           Warden.Data.Numeric

fromNumericFieldSummary :: NumericFieldSummary -> Value
fromNumericFieldSummary NoNumericFieldSummary = object [
    "type" .= String "no-numeric-field-summary"
  ]
fromNumericFieldSummary (NumericFieldSummary nss) = object [
    "type" .= String "numeric-field-summary"
  , "field-summary" .= Array (V.map fromNumericSummary nss)
  ]

toNumericFieldSummary :: Value -> Parser NumericFieldSummary
toNumericFieldSummary (Object o) =
  o .: "type" >>= \case
    String "no-numeric-field-summary" ->
      pure NoNumericFieldSummary
    String "numeric-field-summary" ->
      o .: "field-summary" >>= \case
        (Array as) ->
          fmap NumericFieldSummary $ V.mapM toNumericSummary as
        x -> typeMismatch "Warden.Data.Numeric.NumericFieldSummary.field-summary" x
    String s ->
      fail . T.unpack $ "invalid NumericFieldSummary type: " <> s
    x -> typeMismatch "Warden.Data.NumericFieldSummary.type" x
toNumericFieldSummary x = typeMismatch "Warden.Data.Numeric.NumericFieldSummary" x

fromNumericSummary :: NumericSummary -> Value
fromNumericSummary NoNumericSummary = object [
    "type" .= String "no-numeric-summary"
  ]
fromNumericSummary (NumericSummary mn mx mean s md) = object [
    "type" .= String "numeric-summary"
  , "minimum" .= fromMinimum mn
  , "maximum" .= fromMaximum mx
  , "mean" .= fromMean mean
  , "stddev" .= fromStdDev s
  , "median" .= fromMedian md
  ]

toNumericSummary :: Value -> Parser NumericSummary
toNumericSummary (Object o) =
  (o .: "type" :: Parser Text) >>= \case
    "numeric-summary" -> NumericSummary
      <$> (toMinimum =<< (o .: "minimum"))
      <*> (toMaximum =<< (o .: "maximum"))
      <*> (toMean =<< (o .: "mean"))
      <*> (toStdDev =<< (o .: "stddev"))
      <*> (toMedian =<< (o .: "median"))
    "no-numeric-summary" -> pure NoNumericSummary
    s -> fail . T.unpack $ "invalid NumericSummary type: " <> s
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
