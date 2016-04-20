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
prop_phoneNumberP_neg n =
  let n' = T.encodeUtf8 $ renderIntegral n
      r = parseOnly phoneNumberP n' in
  isLeft r === True

return []
tests :: IO Bool
tests = $quickCheckAll
