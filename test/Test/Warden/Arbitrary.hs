{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Warden.Arbitrary where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Csv
import qualified Data.Set as S
import           Data.List (nub)
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Word

import           Debruijn.Hex (parseHex)

import           Disorder.Core (utf8BS)
import           Disorder.Corpus

import           P

import           Prelude (fromEnum)

import           System.FilePath (joinPath)

import           Test.Delorean.Arbitrary ()
import           Test.QuickCheck (Small(..), NonNegative(..))
import           Test.QuickCheck (Arbitrary, Gen, elements, choose, listOf, listOf1)
import           Test.QuickCheck (vectorOf, arbitrary, suchThat, oneof, sized)
import           Test.QuickCheck.Instances ()

import           Text.Printf (printf)

import           Warden.Data
import           Warden.Parser.PII

newtype ValidRow =
  ValidRow {
    unValidRow :: Row
  } deriving (Eq, Show)

instance Arbitrary ValidRow where
  arbitrary = fmap (ValidRow . SVFields) $ genRows
    where
      genRows = fmap V.fromList $ listOf1 utf8BS

instance Arbitrary Separator where
  arbitrary = elements $ Separator <$> filter (not . affectsRowState) [32..127]

-- | Valid rows for testing the tokenizer.
newtype ValidSVRow = ValidSVRow { getValidSVRow :: [BS.ByteString] }
  deriving (Eq, Show, Ord, ToRecord)

instance Arbitrary RowCount where
  arbitrary = fmap (RowCount . getSmall . getNonNegative) $ arbitrary

instance Arbitrary FieldCount where
  arbitrary = fmap FieldCount $ arbitrary `suchThat` (>= 2)

-- Bytes which can break the row-statelessness of the parser.
affectsRowState :: Word8 -> Bool
affectsRowState w = elem w $ special
 where
  special :: [Word8]
  special = (fromIntegral . ord) <$> ['"', '\'', '\r', '\n', '\\']

invalidSVRow :: Separator -> Gen BL.ByteString
invalidSVRow (Separator s) = BL.intercalate (BL.pack [s]) <$> listOf1 invalidSVField

invalidSVField :: Gen BL.ByteString
invalidSVField = BL.pack <$> (listOf arbitrary) `suchThat` isInvalidText
 where
  isInvalidText bs = (isLeft . decodeUtf8' . BS.pack) bs && not (any affectsRowState bs)

validSVField :: Separator
             -> Gen BS.ByteString
validSVField (Separator s) = utf8BS `suchThat` isValid
  where
    isValid bs =
      let bs' = BS.unpack bs in
         all (/= s) bs'
      && not (any affectsRowState bs')

validSVFieldQuotes :: Separator
                   -> Gen BS.ByteString
validSVFieldQuotes (Separator s) = utf8BS `suchThat` isValid
  where
    isValid bs =
      let bs' = BS.unpack bs in
         all (/= s) bs'
      && not (any forbidden bs')

    forbidden c = affectsRowState c && c /= (fromIntegral $ ord '"')

validSVRow :: Separator -> FieldCount -> Gen ValidSVRow
validSVRow s (FieldCount n) = ValidSVRow <$> vectorOf n (validSVField s)

validSVRowQuotes :: Separator -> FieldCount -> Gen ValidSVRow
validSVRowQuotes s (FieldCount n) = ValidSVRow <$> vectorOf n (validSVFieldQuotes s)

data TestField =
    TestIntegral Integer
  | TestReal Double
  | TestText Text
  | TestBoolean Bool
  deriving (Eq, Show)

packTestField :: TestField -> BSC.ByteString
packTestField (TestIntegral n) = BSC.pack $ show n
packTestField (TestReal n) = BSC.pack $ show n
packTestField (TestText t) = encodeUtf8 t
packTestField (TestBoolean b) = BSC.pack $ show b

tokenizedRow :: FieldCount -> Gen Row
tokenizedRow (FieldCount n) = (SVFields . V.fromList) <$>
  liftM packTestField <$> (vectorOf n (arbitrary :: Gen TestField))

instance Arbitrary ParsedField where
  arbitrary = elements [minBound..maxBound]

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
  arbitrary = oneof [
      Minimum <$> arbitrary
    , pure NoMinimum
    ]

instance Arbitrary Maximum where
  arbitrary = oneof [
      Maximum <$> arbitrary
    , pure NoMaximum
    ]

instance Arbitrary Mean where
  arbitrary = Mean <$> arbitrary

instance Arbitrary StdDev where
  arbitrary = StdDev <$> (arbitrary `suchThat` (> 0.0))

instance Arbitrary Median where
  arbitrary = oneof [
      Median <$> arbitrary
    , pure NoMedian
    ]

instance Arbitrary NumericSummary where
  arbitrary = oneof [
      pure NoNumericSummary
    , NumericSummary <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
    ]

instance Arbitrary ReservoirSize where
  arbitrary = ReservoirSize <$> choose (1, 100)

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

instance Arbitrary ViewFile where
  arbitrary = do
    v <- arbitrary
    d <- arbitrary
    fp <- (FilePart . T.pack . joinPath . fmap T.unpack) <$> listOf1 (elements viruses)
    pure $ ViewFile v d fp

newtype NPlus =
  NPlus {
    unNPlus :: Int
  } deriving (Eq, Show, Ord, Num)

instance Arbitrary NPlus where
  arbitrary = NPlus <$> choose (1, 10000)

newtype UnitReal =
  UnitReal {
    unUnitReal :: Double
  } deriving (Eq, Show)

instance Arbitrary UnitReal where
  arbitrary = UnitReal <$> choose (0.0, 1.0)

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

genLooksVec :: Gen (V.Vector (VU.Vector ObservationCount))
genLooksVec = fmap V.fromList $ listOf1 genFieldLooks
  where
    genFieldLooks = do
      vs <- vectorOf (length ([minBound..maxBound] :: [FieldLooks])) arbitrary
      pure $ VU.fromList vs

instance Arbitrary FieldLookCount where
  arbitrary = oneof [pure NoFieldLookCount, fmap FieldLookCount genLooksVec]

instance Arbitrary UniqueTextCount where
  arbitrary = oneof [
      pure LooksFreeform
   , (UniqueTextCount . S.fromList) <$> arbitrary
   ]

instance Arbitrary TextCounts where
  arbitrary = oneof [
      pure NoTextCounts
    , ((TextCounts . V.fromList . NE.toList) <$> arbitrary)
    ]

instance Arbitrary SVParseState where
  arbitrary = SVParseState <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> pure NoFieldReservoirAcc
                           <*> arbitrary

instance Arbitrary WardenVersion where
  arbitrary = WardenVersion <$> elements southpark

instance Arbitrary ViewMetadata where
  arbitrary = ViewMetadata <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance Arbitrary ViewMarker where
  arbitrary = ViewMarker <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

passedViewMarker :: Gen ViewMarker
passedViewMarker = do
  results <- listOf1 arbitrary
  let results' = fmap (\r -> r { summaryStatus = MarkerPass }) results
  mark <- arbitrary
  pure $ mark { vmCheckResults = results' }

failedViewMarker :: Gen ViewMarker
failedViewMarker = do
  results <- listOf1 arbitrary `suchThat` (not . all ((== MarkerPass) . summaryStatus))
  mark <- arbitrary
  pure $ mark { vmCheckResults = results }

instance Arbitrary ChunkCount where
  arbitrary = (ChunkCount . unNPlus) <$> (arbitrary `suchThat` ((< 1000) . unNPlus))

instance Arbitrary FieldType where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary FieldForm where
  arbitrary = oneof [
      pure FreeForm
    , CategoricalForm <$> arbitrary
    ]

instance Arbitrary FieldUniques where
  arbitrary = fmap (FieldUniques . getSmall . getNonNegative) arbitrary

instance Arbitrary SchemaField where
  arbitrary = SchemaField <$> arbitrary <*> arbitrary

instance Arbitrary SchemaVersion where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Schema where
  arbitrary = Schema <$> arbitrary <*> arbitrary

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

instance Arbitrary SamplingType where
  arbitrary = oneof [
      pure NoSampling
    , ReservoirSampling <$> arbitrary
    ]

instance Arbitrary CheckParams where
  arbitrary = CheckParams <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance Arbitrary NumCPUs where
  arbitrary = (NumCPUs . unNPlus) <$> arbitrary

-- FIXME: expose test generators from debruijn
instance Arbitrary RunId where
  arbitrary = fmap RunId $
    forceParse =<< (fmap T.pack $ vectorOf 16 hex)

    where
      hex = elements hexes

      hexes = ['a'..'f'] <> ['0'..'9']

      forceParse t = case parseHex t of
        Nothing -> fail . T.unpack $ "hex " <> t <> " failed to parse"
        Just h -> pure h

instance Arbitrary WardenParams where
  arbitrary = WardenParams <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary

renderedBool :: Gen Text
renderedBool =
  let reps = [
               "t"
             , "true"
             , "f"
             , "false"
             , "True"
             , "False"
             ] in
  elements . nub $ reps <> (T.toUpper <$> reps)

renderedNonBool :: Gen Text
renderedNonBool = arbitrary `suchThat` notBool
  where
    notBool t = not . flip elem bools $ T.toLower t

    bools = [
        "t"
      , "f"
      , "true"
      , "false"
      ]

instance Arbitrary CompatibleEntries where
  arbitrary = (CompatibleEntries . getSmall . getNonNegative) <$> arbitrary

instance Arbitrary FieldHistogram where
  arbitrary = fmap (FieldHistogram . VU.fromList) $
    (vectorOf (length ([minBound..maxBound] :: [FieldType])) arbitrary) `suchThat` (any (> (CompatibleEntries 0)))

-- No abnormalities, the happy case.
validHistogramPair :: Gen (RowCount, FieldHistogram)
validHistogramPair = do
  n <- fmap (fromIntegral . unNPlus) $ arbitrary
  let rc = RowCount n
  let nText = CompatibleEntries n
  nOthers <- replicateM (length ([minBound..maxBound] :: [FieldType])) (upTo n)
  let h = VU.fromList $ nText : nOthers
  let h' = FieldHistogram . VU.update h $ VU.fromList [(fromEnum TextField, nText)]
  pure (rc, h')
  where
    upTo n = fmap CompatibleEntries $ choose (1, n)

-- Example of field type in the middle of the tree.
realHistogramPair :: Gen (RowCount, FieldHistogram)
realHistogramPair = do
  (rc, FieldHistogram h) <- validHistogramPair
  let nReal = CompatibleEntries $ unRowCount rc
  let h' = FieldHistogram . VU.update h $ VU.fromList [(fromEnum RealField, nReal)]
  pure (rc, h')

-- Example of field type at the bottom of the tree.
booleanHistogramPair :: Gen (RowCount, FieldHistogram)
booleanHistogramPair = do
  (rc, FieldHistogram h) <- validHistogramPair
  let nBoolean = CompatibleEntries $ unRowCount rc
  let h' = FieldHistogram . VU.update h $
             VU.fromList [
                 (fromEnum BooleanField, nBoolean)
               ]
  pure (rc, h')
  
instance Arbitrary FieldMatchRatio where
  arbitrary = fmap (FieldMatchRatio . unUnitReal) arbitrary

instance Arbitrary TextFreeformThreshold where
  arbitrary = fmap (TextFreeformThreshold . unNPlus) arbitrary

instance Arbitrary FieldIndex where
  arbitrary = fmap (FieldIndex . getSmall . getNonNegative) $ arbitrary

instance Arbitrary ExitType where
  arbitrary = elements [ExitWithCheckStatus, ExitWithSuccess]

instance Arbitrary IncludeDotFiles where
  arbitrary = elements [IncludeDotFiles, NoIncludeDotFiles]

smallPositiveEven :: Gen Int
smallPositiveEven = fmap (* 2) (choose (1, 20))

instance Arbitrary KAcc where
  arbitrary = fmap (KAcc . unNPlus) arbitrary

instance Arbitrary StdDevAcc where
  arbitrary = fmap StdDevAcc (choose (0.0, 10000.0))

instance Arbitrary MeanAcc where
  arbitrary = fmap MeanAcc arbitrary

instance Arbitrary MeanDevAcc where
  arbitrary = oneof [
      pure MeanDevInitial
    , MeanDevAcc <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary NumericState where
  arbitrary = NumericState <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RowCountSummary where
  arbitrary =
    RowCountSummary
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary FieldNumericState where
  arbitrary = oneof [
      pure NoFieldNumericState
    , (FieldNumericState . V.fromList) <$> (listOf1 arbitrary)
    ]

instance Arbitrary NumericFieldSummary where
  arbitrary = oneof [
      pure NoNumericFieldSummary
    , (NumericFieldSummary . V.fromList) <$> listOf1 arbitrary
    ]

instance Arbitrary MStdDevAcc where
  arbitrary = oneof [
      pure NoStdDevAcc
    , MStdDevAcc <$> arbitrary
    ]

genEmail :: Gen BS.ByteString
genEmail = do
  sep <- elements ["-", ".", "+"]
  name <- fmap (T.intercalate sep . fmap (T.filter (/= ' '))) $ listOf1 (elements muppets)
  host <- fmap (T.intercalate sep . fmap (T.filter (/= ' '))) $ listOf1 (elements viruses)
  tld <- fmap (T.filter (/= ' ')) $ elements southpark
  pure . encodeUtf8 $ T.concat [name, "@", host, ".", tld]

genPhoneNumber :: Gen BS.ByteString
genPhoneNumber =
  oneof [australianPhoneNumber, internationalPhoneNumber]

australianPhoneNumber :: Gen BS.ByteString
australianPhoneNumber = do
  fd <- fmap T.pack $ vectorOf 1 (elements ['2', '3', '4', '7', '8'])
  fmap (encodeUtf8 . (("0" <> fd) <>) . T.concat) $ vectorOf 8 phoneDigit

internationalPhoneNumber :: Gen BS.ByteString
internationalPhoneNumber = do
  fd <- fmap T.pack $ vectorOf 2 (choose ('0', '9'))
  fmap (encodeUtf8 . (("+" <> fd) <>) . T.concat) $ vectorOf 9 phoneDigit

phoneDigit :: Gen Text
phoneDigit =
  oneof [sepped, unsepped]
  where
    sepped = do
      sep <- elements ['-', '.']
      d <- choose ('0', '9')
      pure $ T.pack [sep, d]

    unsepped = do
      d <- choose ('0', '9')
      pure . T.pack $ pure d

genAddress :: Gen BS.ByteString
genAddress = do
  num <- numericPart
  name <- namePart
  street <- streetPart
  suffix <- oneof [pure "", fmap ((" " <>) . encodeUtf8) (elements muppets)]
  pure $ (BSC.intercalate " " [num, name, street]) <> suffix
  where
    numericPart = oneof [wholeNum, splitNum]

    wholeNum = intBS

    splitNum = do
      a <- intBS
      b <- intBS
      pure $ BSC.concat [a, "/", b]

    namePart = fmap encodeUtf8 $ elements muppets

    streetPart = elements . concatMap munge $ fmap decodeUtf8 streetTypes'

    streetTypes' = streetTypes <> extraStreetTypes

    -- Not included in the list in src, make sure they still parse okay.
    extraStreetTypes = [
        "street"
      , "crescent"
      , "avenue"
      ]

    munge st = fmap encodeUtf8 $ concatMap mungePunctuation [
        st
      , T.toUpper st
      , T.toTitle st
      ]

    mungePunctuation st = [
        st
      , st <> "."
      ]

    intBS = fmap (encodeUtf8 . renderIntegral . unNPlus) arbitrary

genPII :: Gen (BS.ByteString, PIIType)
genPII = oneof [
    (fmap (flip (,) PhoneNumber)) genPhoneNumber
  , (fmap (flip (,) EmailAddress)) genEmail
  , (fmap (flip (,) Address)) genAddress
  , (fmap (flip (,) CreditCard)) genCreditCard
  ]

genCreditCard :: Gen BS.ByteString
genCreditCard = do
  ds <- fmap (BS.concat . fmap (encodeUtf8 . renderIntegral)) genCreditCardDigits
  oneof [
      pure ds
    , mangle ds
    ]
  where
    genCreditCardDigits :: Gen [Int]
    genCreditCardDigits = do
      ccLen <- choose (12, 19)
      mainDigs <- vectorOf ccLen $ choose (0, 9)
      pure $ mainDigs <> [luhnCheckDigit mainDigs]

    luhnCheckDigit ds =
      case (luhn ds) `mod` 10 of
        0 -> 0
        n -> 10 - n

    luhn =
      fst . foldl luhn' (0, True) . reverse

    luhn' (acc, False) d =
      (acc + d, True)
    luhn' (acc, True) d =
      let d' = if (d * 2) > 9
                 then (d * 2) - 9
                 else d * 2 in
      (acc + d', False)

    mangle ds = do
      filler <- fmap BS.singleton $ elements [0x20, 0x2d, 0x2e] -- space, hyphen, period
      pure . BS.intercalate filler $ splitEvery 4 ds

    splitEvery n bs =
      if BS.null bs
        then pure bs
        else
          let c = BS.take n bs
              bs' = BS.drop n bs in
          c : (splitEvery n bs')

genNonCreditCard :: Gen BS.ByteString
genNonCreditCard = oneof [tooShort, tooLong, badLuhn]
  where
    tooShort = do
      n <- choose (0, 11)
      cc <- genCreditCard
      pure $ BS.take n cc

    tooLong = do
      n <- choose (26, 50)
      fmap BS.pack $ vectorOf n arbitrary

    -- Insert a single-digit error into a valid CC number, which the Luhn
    -- check should find.
    badLuhn = do
      cc <- genCreditCard
      ix <- choose (0, (BS.length cc) - 1) `suchThat` (\ix -> digit (BS.index cc ix))
      let old = BS.index cc ix
      new <- fmap BS.singleton $ choose (0x30, 0x39) `suchThat` (/= old) -- 1-9
      let (l, r) = (BS.take ix cc, BS.drop (ix+1) cc)
      pure $ BS.concat [l, new, r]

    digit c = c >= 0x30 && c <= 0x39

nonPhoneNumber :: Gen BS.ByteString
nonPhoneNumber = oneof [tooShort, tooLong, leadingZeroes]
  where
    tooShort = do
      n <- choose (0, 9)
      digits <- vectorOf n $ elements [0x30..0x39]
      pure $ BS.pack digits

    tooLong = do
      n <- choose (11, 20)
      digits <- vectorOf n $ elements [0x30..0x39]
      pure $ BS.pack digits

    leadingZeroes = do
      n <- choose (2, 10)
      digits <- vectorOf (10 - n) $ elements [0x30..0x39]
      pure . BS.pack $ (L.take n $ L.repeat 0x30) <> digits

instance Arbitrary MaxPIIObservations where
  arbitrary = fmap MaxPIIObservations $ choose (1000, 1000000)

instance Arbitrary PIIObservations where
  arbitrary = oneof [
      pure NoPIIObservations
    , pure TooManyPIIObservations
    , PIIObservations <$> arbitrary
    ]

instance Arbitrary PotentialPII where
  arbitrary = PotentialPII <$> arbitrary <*> arbitrary

instance Arbitrary PIIType where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary FileFormat where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary PIICheckType where
  arbitrary = oneof [
      pure NoPIIChecks
    , PIIChecks <$> arbitrary
    ]
