{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.PII where

import qualified Data.Text.Encoding as T

import           Disorder.Corpus (southpark)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.PII

prop_checkPII_pos :: Property
prop_checkPII_pos = forAll genPII $ \bs ->
  let r = checkPII bs in
  isJust r === True

prop_checkPII_neg :: Integer -> Property
prop_checkPII_neg n = forAll (elements southpark) $ \bs ->
  let r1 = checkPII bs
      r2 = checkPII . T.encodeUtf8 $ renderIntegral n in
  (r1, r2) === (Nothing, Nothing)

return []
tests :: IO Bool
tests = $quickCheckAll
