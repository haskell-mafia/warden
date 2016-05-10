{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Parser.PII where

import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Parser.PII
import           Warden.Row.Internal (asciiToLower)

prop_emailP_pos :: Property
prop_emailP_pos = forAll genEmail $ \e ->
  let r = parseOnly emailP e in
  isRight r === True

prop_emailP_neg :: Property
prop_emailP_neg = forAll (elements muppets) $ \e ->
  let r = parseOnly emailP e in
  isLeft r === True

prop_phoneNumberP_pos :: Property
prop_phoneNumberP_pos = forAll genPhoneNumber $ \n ->
  let r = parseOnly phoneNumberP n in
  isRight r === True

prop_phoneNumberP_neg :: Int -> Property
prop_phoneNumberP_neg n = forAll nonPhoneNumber $ \m ->
  let n' = T.encodeUtf8 $ renderIntegral n
      r1 = parseOnly phoneNumberP n'
      r2 = parseOnly phoneNumberP m in
  (isLeft r1, isLeft r2) === (True, True)

prop_addressP_pos :: Property
prop_addressP_pos = forAll (asciiToLower <$> genAddress) $ \a ->
  let r = parseOnly addressP a in
  isRight r === True

prop_addressP_neg :: NPlus -> Property
prop_addressP_neg (NPlus n) = forAll (elements muppets) $ \m ->
  let bs1 = T.encodeUtf8 $ m <> " " <> (renderIntegral n)
      bs2 = T.encodeUtf8 m
      bs3 = T.encodeUtf8 $ renderIntegral n
      rs = (parseOnly addressP) <$> [bs1, bs2, bs3] in
  all isLeft rs === True

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
