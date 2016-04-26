{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Parser.Row.RFC4180 where

import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Parser.Row.RFC4180

prop_unescapedFieldP :: Separator -> Property
prop_unescapedFieldP s = forAll (validSVField s) $ \f ->
  (parseOnly (rawFieldP s) f) === Right f

prop_escapedFieldP :: Separator -> Property
prop_escapedFieldP s = forAll (validSVField s) $ \f ->
  (parseOnly (rawFieldP s) ("\"" <> f <> "\"")) === Right f

prop_escapedFieldP_infix :: Separator -> Property
prop_escapedFieldP_infix s = forAll (validSVField s) $ \f ->
  let s' = BS.pack . pure $ unSeparator s
      middle = f <> s' <> f <> "\"\"" <> s' <> f in
  (parseOnly (rawFieldP s) ("\"" <> middle <> "\"")) === Right middle

return []
tests :: IO Bool
tests = $quickCheckAll
