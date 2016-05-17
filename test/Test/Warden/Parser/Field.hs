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
tests = $quickCheckAll
