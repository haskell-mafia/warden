{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Row.Parser where

import           Data.Attoparsec.ByteString (parseOnly, notWord8)
import qualified Data.ByteString as BS

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Row.Parser

prop_sepByByte1P :: Separator -> FieldCount -> Property
prop_sepByByte1P s c = forAll (getValidSVRow <$> validSVRow s c) $ \bs ->
  let bs' = BS.intercalate (BS.pack . pure $ unSeparator s) bs
      r = parseOnly (sepByByte1P (many (notWord8 (unSeparator s))) s) bs' in
  r === Right (BS.unpack <$> bs)

prop_unescapedFieldP :: Separator -> Property
prop_unescapedFieldP s = forAll (unquotedField s) $ \f ->
  (parseOnly (rawFieldP s) f) === Right f

prop_escapedFieldP :: Separator -> Property
prop_escapedFieldP s = forAll (unquotedField s) $ \f ->
  (parseOnly (rawFieldP s) ("\"" <> f <> "\"")) === Right f

prop_escapedFieldP_infix :: Separator -> Property
prop_escapedFieldP_infix s = forAll (unquotedField s) $ \f ->
  let s' = BS.pack . pure $ unSeparator s
      middle = f <> s' <> f <> "\"\"" <> s' <> f in
  (parseOnly (rawFieldP s) ("\"" <> middle <> "\"")) === Right middle

return []
tests :: IO Bool
tests = $quickCheckAll
