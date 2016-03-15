{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Check(
    fromCheckDescription
  , toCheckDescription
  , fromVerbosity
  , toVerbosity
  ) where

import           Data.Aeson.Types (Value(..), Parser, typeMismatch)

import qualified Data.Text as T

import           P

import           Warden.Data.Check
import           Warden.Serial.Json.Common

fromCheckDescription :: CheckDescription -> Value
fromCheckDescription d = String $ renderCheckDescription d

toCheckDescription :: Value -> Parser CheckDescription
toCheckDescription (String s) = case parseCheckDescription s of
  Just d -> pure d
  Nothing -> fail $ "invalid check: " <> T.unpack s
toCheckDescription x          = typeMismatch "Warden.Data.Check.CheckDescription" x

fromVerbosity :: Verbosity -> Value
fromVerbosity = String . renderVerbosity

toVerbosity :: Value -> Parser Verbosity
toVerbosity (String s) = fromTextField parseVerbosity "Verbosity" s
toVerbosity x = typeMismatch "Warden.Data.Check.Verbosity" x
