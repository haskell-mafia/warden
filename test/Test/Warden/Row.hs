{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Row where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Row

prop_updateFieldNumericState' :: Int -> Double -> Property
prop_updateFieldNumericState' m n =
  let nb = T.encodeUtf8 $ renderFractional n
      mb = T.encodeUtf8 $ renderIntegral m
      ns = updateFieldNumericState' nb initialNumericState
      ms = updateFieldNumericState' mb initialNumericState in
  (ns == initialNumericState, ms == initialNumericState) === (False, False)

prop_asciiToLower :: Property
prop_asciiToLower = forAll (elements muppets) $ \t ->
  let bs = T.encodeUtf8 $ T.toUpper t
      r = T.decodeUtf8 $ asciiToLower bs in
  r === t

prop_asciiToLower_numbers :: Double -> Property
prop_asciiToLower_numbers n =
  let bs = T.encodeUtf8 $ renderFractional n
      r = asciiToLower bs in
  r === bs

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
