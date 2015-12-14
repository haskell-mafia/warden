{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Warden where

import           Data.Attoparsec.Text

import           Disorder.Aeson
import           Disorder.Core.Tripping

import           P

import           System.IO
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary ()

import           Warden.Data

prop_roundtrip_parsed_field :: ParsedField -> Property
prop_roundtrip_parsed_field = tripping renderParsedField (parseOnly field)

prop_roundtrip_json_numericsummary :: NumericSummary -> Property
prop_roundtrip_json_numericsummary = jsonProp

monoidId :: (Monoid a, Show a, Eq a) => a -> Property
monoidId mn =
       (mn <> mempty === mn)
  .&&. (mempty <> mn === mn)

prop_minimum_monoid_id :: Minimum -> Property
prop_minimum_monoid_id = monoidId

prop_maximum_monoid_id :: Maximum -> Property
prop_maximum_monoid_id = monoidId

return []
tests :: IO Bool
tests = $quickCheckAll
