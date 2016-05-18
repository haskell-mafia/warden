{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Parser.Field where

import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Parser.Field

prop_checkFieldBool_pos :: Property
prop_checkFieldBool_pos = forAll (T.encodeUtf8 <$> renderedBool) $ \bs ->
  let r = checkFieldBool bs in
  r === True

prop_checkFieldBool_neg :: Property
prop_checkFieldBool_neg = forAll (T.encodeUtf8 <$> renderedNonBool) $ \bs ->
  let r = checkFieldBool bs in
  r === False

prop_checkFieldNumeric_pos :: Int -> Double -> Property
prop_checkFieldNumeric_pos n m =
  let bs1 = T.encodeUtf8 $ renderIntegral n
      bs2 = T.encodeUtf8 $ renderFractional m
      r1 = checkFieldNumeric bs1
      r2 = checkFieldNumeric bs2 in
  (r1, r2) === (Just' LooksIntegral, Just' LooksReal)

prop_checkFieldNumeric_neg :: Int -> Double -> Property
prop_checkFieldNumeric_neg n m = forAll (elements muppets) $ \b ->
  let p1 = T.encodeUtf8 $ renderIntegral n
      p2 = T.encodeUtf8 $ renderFractional m
      b' = T.encodeUtf8 b
      bss = [
          b'
        , b' <> p1
        , b' <> p2
        , p1 <> b'
        , p2 <> b'
        , b' <> p1 <> b'
        , b' <> p2 <> b'
        , p1 <> b' <> p1
        , p2 <> b' <> p2
        ]
      r = all (== Nothing') $ checkFieldNumeric <$> bss in
  r === True

-- Unit tests for edge cases QC might take a while to stumble upon.
prop_checkFieldNumeric_neg_weird :: Property
prop_checkFieldNumeric_neg_weird =
  let bs1 = "1.2e"
      bs2 = "1e+"
      bss = [bs1, bs2]
      r = all (== Nothing') $ checkFieldNumeric <$> bss in
  once $ r === True

prop_numericFieldP_pos :: Int -> Double -> Property
prop_numericFieldP_pos n m =
  let tn = T.encodeUtf8 $ renderIntegral n
      tm = T.encodeUtf8 $ renderFractional m
      n' = parseOnly numericFieldP tn
      m' = parseOnly numericFieldP tm in
  (Right . NumericField $ fromIntegral n, Right $ NumericField m) === (n', m')

prop_numericFieldP_neg :: Property
prop_numericFieldP_neg = forAll (elements muppets) $ \t ->
  let r = parseOnly numericFieldP $ T.encodeUtf8 t in
  isLeft r === True

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
