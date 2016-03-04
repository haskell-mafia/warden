{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Test.Warden.Arbitrary where

import           Data.AEq (AEq, (===), (~==))
import           Data.Array (Array, array)
import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Csv
import           Data.List (zip)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text            (Text)
import qualified Data.Text as T
import           Data.Text.Encoding   (decodeUtf8, decodeUtf8')
import           Data.Vector (Vector)
import qualified Data.Vector          as V
import           Data.Word

import           Disorder.Core (utf8BS, genValidUtf8)
import           Disorder.Corpus

import           Lane.Data (dateAsPartition)

import           P

import           System.FilePath (joinPath, (</>))

import           Test.Delorean.Arbitrary ()
import           Test.QuickCheck (Arbitrary, Gen, elements, choose, listOf, listOf1)
import           Test.QuickCheck (vectorOf, arbitrary, suchThat, oneof, sized)
import           Test.QuickCheck.Instances ()

import           Text.Printf (printf)

import           Warden.Data
import           Warden.Sampling.Reservoir

instance AEq Mean where
  (Mean x) === (Mean y) = x === y
  (Mean x) ~== (Mean y) = x ~== y

instance AEq StdDev where
  (StdDev x) === (StdDev y) = x === y
  (StdDev x) ~== (StdDev y) = x ~== y

newtype ValidRow =
  ValidRow {
    unValidRow :: Row
  } deriving (Eq, Show)

instance Arbitrary ValidRow where
  arbitrary = fmap (ValidRow . SVFields) $ genRows
    where
      genRows = fmap V.fromList $ listOf1 genValidUtf8

instance Arbitrary Separator where
  arbitrary = elements $ Separator <$> filter (not . affectsRowState) [32..127]

-- | Valid rows for testing the tokenizer.
newtype ValidSVRow = ValidSVRow { getValidSVRow :: [Text] }
  deriving (Eq, Show, Ord, ToRecord)

instance Arbitrary RowCount where
  arbitrary = RowCount <$> choose (1, 100)

instance Arbitrary FieldCount where
  arbitrary = fmap FieldCount $ arbitrary `suchThat` (>= 2)

-- Bytes which can break the row-statelessness of the parser.
affectsRowState :: Word8 -> Bool
affectsRowState w = elem w $ special
 where
  special :: [Word8]
  special = (fromIntegral . ord) <$> ['"', '\'', '\r', '\n', '\\']

invalidSVRow :: Separator -> Gen ByteString
invalidSVRow (Separator s) = BL.intercalate (BL.pack [s]) <$> listOf1 invalidSVField

invalidSVField :: Gen ByteString
invalidSVField = BL.pack <$> (listOf arbitrary) `suchThat` isInvalidText
 where
  isInvalidText bs = (isLeft . decodeUtf8' . BS.pack) bs && not (any affectsRowState bs)

validSVField :: Separator
             -> Gen Text
validSVField (Separator s) = decodeUtf8 <$>
  utf8BS `suchThat` isValid
  where
    isValid bs =
         let bs' = BS.unpack bs in
         all (/= s) bs'
      && not (any affectsRowState bs')

validSVRow :: Separator -> FieldCount -> Gen ValidSVRow
validSVRow s (FieldCount n) = ValidSVRow <$> vectorOf n (validSVField s)

data TestField =
    TestIntegral Integer
  | TestReal Double
  | TestText Text
  | TestBoolean Bool
  deriving (Eq, Show)

renderTestField :: TestField -> Text
renderTestField (TestIntegral n) = T.pack $ show n
renderTestField (TestReal n) = T.pack $ show n
renderTestField (TestText t) = t
renderTestField (TestBoolean b) = T.pack $ show b

tokenizedRow :: FieldCount -> Gen Row
tokenizedRow (FieldCount n) = (SVFields . V.fromList) <$>
  liftM renderTestField <$> (vectorOf n (arbitrary :: Gen TestField))

instance Arbitrary ParsedField where
  arbitrary = elements [
      ParsedIntegral
    , ParsedReal
    , ParsedText
    , ParsedBoolean
    ]

instance Arbitrary TestField where
  arbitrary = oneof [textField, integralField, realField]

textField :: Gen TestField
textField = TestText <$> elements southpark

integralField :: Gen TestField
integralField = TestIntegral <$> (arbitrary :: Gen Integer)

realField :: Gen TestField
realField = TestReal <$> (arbitrary :: Gen Double)

--
-- numeric instances
--

instance Arbitrary Minimum where
  arbitrary = Minimum <$> arbitrary

instance Arbitrary Maximum where
  arbitrary = Maximum <$> arbitrary

instance Arbitrary Mean where
  arbitrary = Mean <$> arbitrary

instance Arbitrary StdDev where
  arbitrary = StdDev <$> (arbitrary `suchThat` (> 0.0))

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

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NE.fromList <$> listOf1 arbitrary

instance Arbitrary CheckStatus where
  arbitrary = oneof [
      pure CheckPassed
    , CheckFailed <$> arbitrary
    ]

newtype CheckStatusPassed =
  CheckStatusPassed {
    unCheckStatusPassed :: CheckStatus
  } deriving (Eq, Show)

instance Arbitrary CheckStatusPassed where
  arbitrary = pure $ CheckStatusPassed CheckPassed

newtype CheckStatusFailed =
  CheckStatusFailed {
    unCheckStatusFailed :: CheckStatus
  } deriving (Eq, Show)

instance Arbitrary CheckStatusFailed where
  arbitrary = (CheckStatusFailed . CheckFailed) <$> arbitrary

instance Arbitrary Failure where
  arbitrary = oneof [SanityCheckFailure <$> arbitrary]

instance Arbitrary Insanity where
  arbitrary = elements [
      EmptyFile
    , IrregularFile
    ]

instance Arbitrary DirName where
  arbitrary = (DirName . T.unpack) <$> elements viruses

instance Arbitrary FileName where
  arbitrary = (FileName . T.unpack) <$> elements southpark

instance Arbitrary View where
  arbitrary = (View . joinPath . fmap T.unpack) <$> listOf1 (elements muppets)

data ValidViewFile = ValidViewFile View ViewFile
  deriving (Eq, Show)

instance Arbitrary ValidViewFile where
  arbitrary = do
    v <- arbitrary
    d <- arbitrary
    fp <- (joinPath . fmap T.unpack) <$> listOf1 (elements viruses)
    let vf = ViewFile $ (unView v) </> (T.unpack $ dateAsPartition d) </> fp
    pure $ ValidViewFile v vf

instance Arbitrary ViewFile where
  arbitrary = do
    (ValidViewFile _ vf) <- arbitrary
    pure vf

newtype NPlus =
  NPlus {
    unNPlus :: Int
  } deriving (Eq, Show, Ord, Num)

instance Arbitrary NPlus where
  arbitrary = NPlus <$> choose (1, 10000)

newtype InvalidDirTree =
  InvalidDirTree {
    unInvalidDirTree :: DirTree
  } deriving (Eq, Show)

instance Arbitrary InvalidDirTree where
  arbitrary = InvalidDirTree <$> sized arbitrary'
    where
      arbitrary' :: Int -> Gen DirTree
      arbitrary' 0 = DirTree <$> arbitrary <*> (pure []) <*> arbitrary
      arbitrary' n = do
        (NPlus m) <- arbitrary
        ds <- replicateM m (arbitrary' $ max (n - m) 0)
        fs <- arbitrary
        lbl <- arbitrary
        pure $ DirTree lbl ds fs

newtype ValidDirTree =
  ValidDirTree {
    unValidDirTree :: DirTree
  } deriving (Eq, Show)

data ViewLevel =
    YearLevel
  | MonthLevel
  | DayLevel1
  | DayLevel2
  deriving (Eq, Show)

dayLabel :: Gen DirName
dayLabel = do
  n <- choose (1, 31)
  pure . DirName $ "day=" <> (printf "%02d" (n :: Int))

monthLabel :: Gen DirName
monthLabel = do
  n <- choose (1, 12)
  pure . DirName $ "month=" <> (printf "%02d" (n :: Int))

yearLabel :: Gen DirName
yearLabel = do
  n <- choose (1600, 9999)
  pure . DirName $ "year=" <> (show (n :: Int))

instance Arbitrary ValidDirTree where
  arbitrary = fmap ValidDirTree $ DirTree <$> arbitrary <*> (listOf1 (arbitrary' YearLevel)) <*> (pure [])
    where
      arbitrary' :: ViewLevel -> Gen DirTree
      arbitrary' DayLevel2 =
        DirTree <$> arbitrary <*> (pure []) <*> (listOf1 arbitrary)
      arbitrary' DayLevel1 = oneof [
          DirTree <$> dayLabel <*> (pure []) <*> (listOf1 arbitrary)
        , DirTree <$> dayLabel <*> (listOf1 (arbitrary' DayLevel2)) <*> (pure [])
        ]
      arbitrary' MonthLevel =
        DirTree <$> monthLabel <*> (listOf1 (arbitrary' DayLevel1)) <*> (pure [])
      arbitrary' YearLevel =
        DirTree <$> yearLabel <*> (listOf1 (arbitrary' MonthLevel)) <*> (pure [])

instance Arbitrary CheckDescription where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary MarkerVersion where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary CheckResultType where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary MarkerFailure where
  arbitrary = (MarkerFailure . NE.fromList) <$> (listOf1 $ elements muppets)

instance Arbitrary MarkerStatus where
  arbitrary = oneof [
      pure MarkerPass
    , MarkerFail <$> arbitrary
    ]

instance Arbitrary CheckResultSummary where
  arbitrary = CheckResultSummary <$> arbitrary
                                 <*> arbitrary
                                 <*> arbitrary

instance Arbitrary FileMarker where
  arbitrary = FileMarker <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

genLooksVec :: Gen (Vector (Array FieldLooks ObservationCount))
genLooksVec = fmap V.fromList $ listOf1 genFieldLooks
  where
    genFieldLooks = do
      pairs <- genLooksPairs
      pure $ array (minBound, maxBound) pairs

    genLooksPairs = do
      vs <- vectorOf (length ([minBound..maxBound] :: [FieldLooks])) arbitrary
      pure $ zip [minBound..maxBound] vs

instance Arbitrary FieldLookCount where
  arbitrary = oneof [pure NoFieldLookCount, fmap FieldLookCount genLooksVec]

instance Arbitrary SVParseState where
  arbitrary = SVParseState <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance Arbitrary ViewMetadata where
  arbitrary = ViewMetadata <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance Arbitrary ViewMarker where
  arbitrary = ViewMarker <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

instance Arbitrary ChunkCount where
  arbitrary = (ChunkCount . unNPlus) <$> (arbitrary `suchThat` ((< 1000) . unNPlus))

instance Arbitrary FieldType where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary SchemaField where
  arbitrary = SchemaField <$> arbitrary

instance Arbitrary SchemaVersion where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Schema where
  arbitrary = Schema <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ObservationCount where
  arbitrary = (ObservationCount . fromIntegral . unNPlus) <$> arbitrary

instance Arbitrary FieldLooks where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Force where
  arbitrary = elements [Force, NoForce]

instance Arbitrary Verbosity where
  arbitrary = elements [Verbose, Quiet]

instance Arbitrary SchemaFile where
  arbitrary = (SchemaFile . T.unpack) <$> elements muppets

instance Arbitrary LineBound where
  arbitrary = (LineBound . unNPlus) <$> arbitrary

instance Arbitrary CheckParams where
  arbitrary = CheckParams <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
