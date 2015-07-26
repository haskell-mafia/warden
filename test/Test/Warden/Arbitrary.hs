{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Warden.Arbitrary where

import P

import qualified Data.ByteString as BS
import Data.Csv
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Test.QuickCheck

newtype SVSep = SVSep { getSVSep :: Word8 }
  deriving (Eq, Show, Ord)

-- | [^a-zA-Z]
instance Arbitrary SVSep where
  arbitrary = elements $ SVSep <$> ([32..64] <> [91..96] <> [123..126])

newtype ValidSVRow = ValidSVRow { getValidSVRow :: [Text] }
  deriving (Eq, Show, Ord, ToRecord)

newtype RowCount = RowCount { getRowCount :: Int }
  deriving (Eq, Show, Ord)

instance Arbitrary RowCount where
  arbitrary = RowCount <$> choose (1, 100)

newtype FieldCount = FieldCount { getFieldCount :: Int }
  deriving (Eq, Show, Ord)

instance Arbitrary FieldCount where
  arbitrary = FieldCount <$> choose (1, 10)

validSVField :: SVSep
             -> Gen Text
validSVField (SVSep s) = (decodeUtf8 . BS.pack) <$>
  (listOf arbitrary) `suchThat` (all validWord)
 where
  -- not the separator, and valid UTF-8/ASCII.
  validWord w = w /= s && w <= 127

validSVRow :: SVSep -> FieldCount -> Gen ValidSVRow
validSVRow s (FieldCount n) = ValidSVRow <$> vectorOf n (validSVField s)
