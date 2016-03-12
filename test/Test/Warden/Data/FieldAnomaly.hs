{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.FieldAnomaly where

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data

-- Text includes everything.
prop_checkFieldType_text :: ObservationCount -> Property
prop_checkFieldType_text rc = forAll arbitrary $ \fl ->
  checkFieldType TextField fl rc === Nothing

return []
tests :: IO Bool
tests = $quickCheckAll
