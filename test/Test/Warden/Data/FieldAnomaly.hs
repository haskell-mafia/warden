{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.FieldAnomaly where

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data

-- Text includes everything.
prop_checkFieldType_text :: RowCount -> Property
prop_checkFieldType_text rc = forAll (arbitrary `suchThat` (not . (== LooksBroken))) $ \fl ->
  checkFieldType TextField fl rc === Nothing

-- Broken is included by nothing.
prop_checkFieldType_broken :: FieldType -> Property
prop_checkFieldType_broken ft = forAll (arbitrary `suchThat` (> (RowCount 0))) $ \rc ->
  (checkFieldType ft LooksBroken rc) === (Just (FieldAnomaly LooksBroken rc))

return []
tests :: IO Bool
tests = $quickCheckAll
