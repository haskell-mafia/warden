{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Warden.Arbitrary where

import P

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Csv
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
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
  arbitrary = FieldCount <$> choose (2, 10)

invalidSVRow :: SVSep -> Gen ByteString
invalidSVRow (SVSep s) = BL.intercalate (BL.pack [s]) <$> listOf1 invalidSVField

invalidSVField :: Gen ByteString
invalidSVField = BL.pack <$> (listOf arbitrary) `suchThat` isInvalidText
 where
  isInvalidText bs = (isLeft . decodeUtf8' . BS.pack) bs && not (any isDelim bs)

  -- Filter out delimiters because including them could make the whole
  -- document fail to parse rather than just the row.
  isDelim w = elem w $ (fromIntegral . ord) <$> ['"', '\'', '\r', '\n']

validSVField :: SVSep
             -> Gen Text
validSVField (SVSep s) = (decodeUtf8 . BS.pack) <$>
  (listOf arbitrary) `suchThat` isValid
 where
  isValid bs = isRight (decodeUtf8' (BS.pack bs)) && all (/= s) bs

validSVRow :: SVSep -> FieldCount -> Gen ValidSVRow
validSVRow s (FieldCount n) = ValidSVRow <$> vectorOf n (validSVField s)
