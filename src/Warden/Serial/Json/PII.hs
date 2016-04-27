{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Serial.Json.PII(
    fromMaxPIIObservations
  , toMaxPIIObservations
  ) where

import           Data.Aeson (toJSON, parseJSON)
import           Data.Aeson.Types (Value(..), Parser, typeMismatch)

import           P

import           Warden.Data.PII

fromMaxPIIObservations :: MaxPIIObservations -> Value
fromMaxPIIObservations = toJSON . unMaxPIIObservations

toMaxPIIObservations :: Value -> Parser MaxPIIObservations
toMaxPIIObservations (Number n) = fmap MaxPIIObservations $ parseJSON (Number n)
toMaxPIIObservations x = typeMismatch "Warden.Data.PII.MaxPIIObservations" x
