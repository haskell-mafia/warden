{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Field where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as S

import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()
import           Test.Warden.Data.Poset.Laws

import           Warden.Data.Field
import           Warden.Data.Poset

prop_FieldType_posetLaws :: (UniquePair FieldType) -> FieldType -> FieldType -> FieldType -> Property
prop_FieldType_posetLaws =
  posetLaws

prop_FieldType_minima :: Property
prop_FieldType_minima =
  minima (S.fromList $ [minBound..maxBound] :: Set FieldType) === S.fromList [IntegralField, BooleanField]

prop_FieldType_minima_exists :: NonEmpty FieldType -> Property
prop_FieldType_minima_exists xs =
  ((minima . S.fromList $ NE.toList xs) /= S.empty) === True

return []
tests :: IO Bool
tests = $quickCheckAll
