{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Parser.Common where

import           Data.Attoparsec.ByteString (parseOnly, notWord8)
import qualified Data.ByteString as BS

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Parser.Common

prop_sepByByte1P :: Separator -> FieldCount -> Property
prop_sepByByte1P s c = forAll (getValidSVRow <$> validSVRow s c) $ \bs ->
  let bs' = BS.intercalate (BS.pack . pure $ unSeparator s) bs
      r = parseOnly (sepByByte1P (many (notWord8 (unSeparator s))) s) bs' in
  r === Right (BS.unpack <$> bs)

return []
tests :: IO Bool
tests = $quickCheckAll
