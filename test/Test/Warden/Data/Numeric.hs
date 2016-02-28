{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Numeric where

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data.Numeric

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
