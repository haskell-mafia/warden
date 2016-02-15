{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Check(
    fromCheckDescription
  , toCheckDescription
  ) where

import           Data.Aeson.Types (Value(..), Parser, typeMismatch)

import           P

import           Warden.Data.Check

fromCheckDescription :: CheckDescription -> Value
fromCheckDescription (CheckDescription d) = String d

toCheckDescription :: Value -> Parser CheckDescription
toCheckDescription (String s) = pure $ CheckDescription s
toCheckDescription x          = typeMismatch "Warden.Data.Check.CheckDescription" x
