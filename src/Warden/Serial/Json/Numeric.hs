{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Numeric (
    toNumericSummary,
    fromNumericSummary
) where

import           Data.Aeson
import           Data.Aeson.Types

import           Warden.Data.Numeric

fromNumericSummary :: NumericSummary -> Value
fromNumericSummary = toJSON

toNumericSummary :: Value -> Parser NumericSummary
toNumericSummary = parseJSON
