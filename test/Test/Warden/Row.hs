{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Row where

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Row

prop_updateFieldNumericState' :: Int -> Double -> Property
prop_updateFieldNumericState' m n =
  let nb = MNumericField $ NumericField n
      mb = MNumericField . NumericField $ fromIntegral m
      ob = NoNumericField
      ns = updateFieldNumericState' nb initialNumericState
      ms = updateFieldNumericState' mb initialNumericState
      os = updateFieldNumericState' ob initialNumericState in
  (ns == initialNumericState, ms == initialNumericState, os == initialNumericState) === (False, False, True)

prop_parseField :: TestField -> Property
prop_parseField tf =
  let bs = packTestField tf
      fl = parseField bs in
  case tf of
    TestIntegral _ -> fl === LooksIntegral
    TestReal _ -> fl === LooksReal
    TestText _ -> fl === LooksText
    TestBoolean _ -> fl === LooksBoolean

return []
tests :: IO Bool
tests = $quickCheckAll
