{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Serial.Json.Numeric where

import           Disorder.Aeson (jsonProp)

import           P

import           System.IO

import           Test.Warden.Arbitrary ()
import           Test.QuickCheck

import           Warden.Data

prop_roundtrip_json_numericsummary :: NumericSummary -> Property
prop_roundtrip_json_numericsummary = jsonProp

return []
tests :: IO Bool
tests = $quickCheckAll
