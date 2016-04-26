{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Parser.Row.DelimitedText where

import           Data.Attoparsec.ByteString (parseOnly)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Parser.Row.DelimitedText

prop_unescapedFieldP :: Separator -> Property
prop_unescapedFieldP s = forAll (validSVField s) $ \f ->
  (parseOnly (rawFieldP s) f) === Right f

-- Quoted fields should be treated the same as everything else.
prop_escapedFieldP :: Separator -> Property
prop_escapedFieldP s = forAll (validSVField s) $ \f ->
  (parseOnly (rawFieldP s) ("\"" <> f <> "\"")) === Right ("\"" <> f <> "\"")

return []
tests :: IO Bool
tests = $quickCheckAll
