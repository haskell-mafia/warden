{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Serial.Json.Numeric where

import           Data.Aeson.Types (parseEither)

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Test.Warden.Arbitrary ()
import           Test.QuickCheck

import           Warden.Data.Numeric
import           Warden.Serial.Json.Numeric

prop_roundtrip_json_numericsummary :: NumericSummary -> Property
prop_roundtrip_json_numericsummary = tripping fromNumericSummary (parseEither toNumericSummary)

return []
tests :: IO Bool
tests = $quickCheckAll
