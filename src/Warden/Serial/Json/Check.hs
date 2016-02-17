{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Check(
    fromCheckDescription
  , toCheckDescription
  ) where

import           Data.Aeson.Types (Value(..), Parser, typeMismatch)

import qualified Data.Text as T

import           P

import           Warden.Data.Check

fromCheckDescription :: CheckDescription -> Value
fromCheckDescription d = String $ renderCheckDescription d

toCheckDescription :: Value -> Parser CheckDescription
toCheckDescription (String s) = case parseCheckDescription s of
  Just d -> pure d
  Nothing -> fail $ "invalid check: " <> T.unpack s
toCheckDescription x          = typeMismatch "Warden.Data.Check.CheckDescription" x
