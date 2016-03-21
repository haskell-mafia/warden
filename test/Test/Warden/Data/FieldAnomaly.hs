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

prop_formAnomalies_fail :: FieldIndex -> Property
prop_formAnomalies_fail fi =
  forAll (choose (2, 50)) $ \n -> 
    forAll (fmap S.fromList $ vectorOf n arbitrary) $ \c ->
      forAll (fmap FieldUniques $ choose (0, (S.size c) - 1)) $ \fu ->
        (isJust $ formAnomalies (CategoricalForm fu) (UniqueTextCount c) fi, isJust $ formAnomalies (CategoricalForm fu) LooksFreeform fi) === (True, True)

return []
tests :: IO Bool
tests = $quickCheckAll
