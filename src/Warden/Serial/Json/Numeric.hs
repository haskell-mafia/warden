{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Serial.Json.Numeric (
    toNumericSummary,
    fromNumericSummary
) where

import           Data.Aeson
import           Data.Aeson.Types

import           P

import           Warden.Data.Numeric

fromNumericSummary :: NumericSummary -> Value
fromNumericSummary (NumericSummary mn mx mean s md) = object [
    "version"  .= ("v1" :: Text)
  , "minimum"  .= mn
  , "maximum"  .= mx
  , "mean"     .= mean
  , "stddev"   .= s
  , "median"   .= md
  ]

toNumericSummary :: Value -> Parser NumericSummary
toNumericSummary (Object o) =
  o .: "version" >>= \case
    "v1" -> NumericSummary
      <$> o .: "minimum"
      <*> o .: "maximum"
      <*> o .: "mean"
      <*> o .: "stddev"
      <*> o .: "median"
    v -> fail $ "Warden.Data.Numeric.NumericSummary: unknown version [" <> v <> "]"
toNumericSummary x = typeMismatch "Warden.Data.Numeric.NumericSummary" x
