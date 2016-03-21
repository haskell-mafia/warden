{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.FieldAnomaly where

import qualified Data.Set as S

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data

-- Text includes everything.
prop_checkFieldType_text :: ObservationCount -> Property
prop_checkFieldType_text rc = forAll arbitrary $ \fl ->
  checkFieldType TextField fl rc === Nothing

prop_formAnomalies_pass :: FieldForm -> FieldIndex -> Property
prop_formAnomalies_pass ff fi = case ff of
  FreeForm ->
    forAll arbitrary $ \c ->
      (formAnomalies ff c fi) === Nothing
  (CategoricalForm u) ->
    forAll (choose (0, unFieldUniques u)) $ \n ->
      forAll (fmap (UniqueTextCount . S.fromList) $ vectorOf n arbitrary) $ \c ->
      (formAnomalies ff c fi) === Nothing

prop_formAnomalies_fail :: FieldUniques -> FieldIndex -> Property
prop_formAnomalies_fail fu fi =
  let ff = CategoricalForm fu in
  forAll (choose ((unFieldUniques fu) + 1, 100000)) $ \n ->
    forAll (fmap (UniqueTextCount . S.fromList) $ vectorOf n arbitrary) $ \c ->
    (isJust $ formAnomalies ff c fi, isJust $ formAnomalies ff LooksFreeform fi) === (True, True)

return []
tests :: IO Bool
tests = $quickCheckAll
