{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Parser.PII where

import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Parser.PII
import           Warden.Row.Internal (asciiToLower)

prop_checkEmail_pos :: Property
prop_checkEmail_pos = forAll genEmail $ \e ->
  checkEmail e === True

prop_checkEmail_neg :: Property
prop_checkEmail_neg = forAll (elements muppets) $ \e ->
  checkEmail e === False

prop_checkPhoneNumber_pos :: Property
prop_checkPhoneNumber_pos = forAll genPhoneNumber $ \n ->
  checkPhoneNumber n === True

prop_checkPhoneNumber_neg :: Int -> Property
prop_checkPhoneNumber_neg n = forAll nonPhoneNumber $ \m ->
  let n' = T.encodeUtf8 $ renderIntegral n
      r1 = checkPhoneNumber n'
      r2 = checkPhoneNumber m in
  (r1, r2) === (False, False)

prop_checkAddress_pos :: Property
prop_checkAddress_pos = forAll (asciiToLower <$> genAddress) $ \a ->
  checkAddress a === True

prop_checkAddress_neg :: NPlus -> Property
prop_checkAddress_neg (NPlus n) = forAll (elements muppets) $ \m ->
  let bs1 = T.encodeUtf8 $ m <> " " <> (renderIntegral n)
      bs2 = T.encodeUtf8 m
      bs3 = T.encodeUtf8 $ renderIntegral n
      rs = checkAddress <$> [bs1, bs2, bs3] in
  and (not <$> rs) === True

prop_checkCreditCard_pos :: Property
prop_checkCreditCard_pos = forAll genCreditCard $ \a ->
  checkCreditCard a === True

prop_checkCreditCard_neg :: Property
prop_checkCreditCard_neg = forAll genNonCreditCard $ \a ->
  checkCreditCard a === False

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
