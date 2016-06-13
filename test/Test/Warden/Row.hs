{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Row where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

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

-- Verify that two invalid UTF-8 fields are still invalid when in a row together.
prop_toRow :: Property
prop_toRow = forAll ((,) <$> invalidSVField <*> invalidSVField) $ \(bs1, bs2) ->
  let row = Right . V.fromList $ fmap BSL.toStrict [bs1, bs2]
      r = toRow row in
  isRowFailure r === True
  where
    isRowFailure (SVFields _) = False
    isRowFailure (RowFailure _) = True

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
