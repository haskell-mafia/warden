{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Test.Warden.Arbitrary where

import           Data.AEq (AEq, (===), (~==))
import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Csv
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8, decodeUtf8')
import qualified Data.Vector          as V
import           Data.Word

import           Disorder.Corpus

import           P

import           Test.QuickCheck (Arbitrary, Gen, elements, choose, listOf, listOf1)
import           Test.QuickCheck (vectorOf, arbitrary, suchThat, oneof)

import           Warden.Data
import           Warden.Sampling.Reservoir

instance AEq Mean where
  (Mean x) === (Mean y) = x === y
  (Mean x) ~== (Mean y) = x ~== y

instance Arbitrary Separator where
  arbitrary = elements $ Separator <$> filter (not . affectsRowState) [32..127]

-- | Valid rows for testing the tokenizer.
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

-- Bytes which can break the row-statelessness of the parser.
affectsRowState :: Word8 -> Bool
affectsRowState w = elem w $ special
 where
  special :: [Word8]
  special = (fromIntegral . ord) <$> ['"', '\'', '\r', '\n', '\\']

-- Get an invalid xSV document by sticking a quote in an inconvenient place.
invalidSVDocument :: Separator -> Gen ByteString
invalidSVDocument (Separator s) = (BL.pack . concat) <$> do
  w1 <- fieldWords
  w2 <- fieldWords
  pure [w1, [fromIntegral (ord '"')], w2]
 where
  fieldWords :: Gen [Word8]
  fieldWords = (listOf1 (elements alphaWords)) `suchThat` (not . (elem s))

  alphaWords :: [Word8]
  alphaWords = [65..91] <> [97..123]

invalidSVRow :: Separator -> Gen ByteString
invalidSVRow (Separator s) = BL.intercalate (BL.pack [s]) <$> listOf1 invalidSVField

invalidSVField :: Gen ByteString
invalidSVField = BL.pack <$> (listOf arbitrary) `suchThat` isInvalidText
 where
  isInvalidText bs = (isLeft . decodeUtf8' . BS.pack) bs && not (any affectsRowState bs)

validSVField :: Separator
             -> Gen Text
validSVField (Separator s) = (decodeUtf8 . BS.pack) <$>
  (listOf arbitrary) `suchThat` isValid
 where
  isValid bs =
       isRight (decodeUtf8' (BS.pack bs))
    && all (/= s) bs
    && not (any affectsRowState bs)

validSVRow :: Separator -> FieldCount -> Gen ValidSVRow
validSVRow s (FieldCount n) = ValidSVRow <$> vectorOf n (validSVField s)

tokenizedRow :: FieldCount -> Gen Row
tokenizedRow (FieldCount n) = (SVFields . V.fromList) <$>
  liftM renderParsedField <$> (vectorOf n (arbitrary :: Gen ParsedField))

instance Arbitrary ParsedField where
  arbitrary = oneof [textField, integralField, realField]

textField :: Gen ParsedField
textField = TextField <$> elements southpark

integralField :: Gen ParsedField
integralField = IntegralField <$> (arbitrary :: Gen Integer)

realField :: Gen ParsedField
realField = RealField <$> (arbitrary :: Gen Double)

--
-- numeric instances
--

instance Arbitrary Minimum where
  arbitrary = Minimum <$> arbitrary

instance Arbitrary Maximum where
  arbitrary = Maximum <$> arbitrary

instance Arbitrary Mean where
  arbitrary = Mean <$> arbitrary

instance Arbitrary Variance where
  arbitrary = Variance . Just <$> (arbitrary `suchThat` (> 0.0))

instance Arbitrary Median where
  arbitrary = Median <$> arbitrary

instance Arbitrary NumericSummary where
  arbitrary = NumericSummary <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary

instance Arbitrary ReservoirSize where
  arbitrary = ReservoirSize <$> choose (1, 100)

instance Arbitrary Seen where
  arbitrary = Seen <$> choose (1, 10000)

instance Arbitrary XDist where
  arbitrary = XDist <$> arbitrary <*> arbitrary

instance Arbitrary Probability where
  arbitrary = Probability <$> choose (0.0, 1.0)

instance AEq Probability where
  (Probability p) === (Probability q) = p === q
  (Probability p) ~== (Probability q) = p ~== q

