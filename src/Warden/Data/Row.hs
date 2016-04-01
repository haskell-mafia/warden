{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Data.Row (
    FieldCount(..)
  , FieldLookCount(..)
  , FieldNumericState(..)
  , LineBound(..)
  , ObservationCount(..)
  , ParsedField(..)
  , RawRecord(..)
  , Row(..)
  , RowCount(..)
  , SVParseState(..)
  , Separator(..)
  , badRows
  , charToSeparator
  , combineFieldLooks
  , emptyLookCountVector
  , fieldLooks
  , initialSVParseState
  , numericState
  , numFields
  , renderFieldCount
  , renderObservationCount
  , renderParsedField
  , renderRowCount
  , resolveSVParseState
  , separatorToChar
  , textCounts
  , totalRows
) where

import           Control.Lens (makeLenses, (^.), (%~))

import           Data.ByteString (ByteString)
import           Data.Char (chr, ord)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Word (Word8)

import           GHC.Generics (Generic)

import           P

import           Prelude (fromEnum)

import           Warden.Data.Field
import           Warden.Data.Numeric
import           Warden.Data.TextCounts

newtype RawRecord =
  RawRecord {
    unRawRecord :: V.Vector ByteString
  } deriving (Eq, Show)

newtype LineBound =
  LineBound {
    unLineBound :: Int
  } deriving (Eq, Show)

newtype FieldCount =
  FieldCount {
    unFieldCount :: Int
  } deriving (Eq, Show, Ord, Num, Generic)

instance NFData FieldCount

renderFieldCount :: FieldCount -> Text
renderFieldCount = renderIntegral . unFieldCount

newtype ObservationCount =
  ObservationCount {
    unObservationCount :: Int64
  } deriving (Eq, Show, Ord, Num, Generic)

$(derivingUnbox "ObservationCount"
  [t| ObservationCount -> Int64 |]
  [| \(ObservationCount x) -> x |]
  [| \x -> (ObservationCount x) |])

instance NFData ObservationCount

renderObservationCount :: ObservationCount -> Text
renderObservationCount (ObservationCount n) = renderIntegral n

newtype Separator =
  Separator {
    unSeparator :: Word8
  } deriving (Eq, Show, Generic)

instance NFData Separator

charToSeparator :: Char -> Separator
charToSeparator = Separator . fromIntegral . ord

separatorToChar :: Separator -> Char
separatorToChar = chr . fromIntegral . unSeparator

-- | Raw record. Can be extended to support JSON objects as well as xSV if
--   needed.
data Row =
    SVFields !(V.Vector ByteString)
  | RowFailure !Text
  deriving (Eq, Show, Generic)

instance NFData Row

newtype RowCount =
  RowCount {
    unRowCount :: Int64
  } deriving (Eq, Show, Ord, Num, Generic)

instance NFData RowCount

renderRowCount :: RowCount -> Text
renderRowCount = T.pack . show . unRowCount

emptyLookCountVector :: VU.Vector ObservationCount
emptyLookCountVector =
  let ixs = fmap fromEnum ([minBound..maxBound] :: [FieldLooks]) in
  VU.replicate (length ixs) (ObservationCount 0)

combineFieldLooks :: FieldLookCount
                  -> FieldLookCount
                  -> FieldLookCount
combineFieldLooks NoFieldLookCount NoFieldLookCount = NoFieldLookCount
combineFieldLooks (FieldLookCount !x) NoFieldLookCount = FieldLookCount x
combineFieldLooks NoFieldLookCount (FieldLookCount !y) = FieldLookCount y
combineFieldLooks (FieldLookCount !x) (FieldLookCount !y) = FieldLookCount . uncurry combine' $ matchSize x y
  where
    combine' = V.zipWith addLooks

    addLooks a b = VU.accumulate (+) a $ VU.indexed b

    -- To retain some sanity in the event of mismatched field counts.
    matchSize a b =
      let la = V.length a
          lb = V.length b
          ln = max la lb
          na = V.concat [a, V.replicate (ln - la) emptyLookCountVector]
          nb = V.concat [b, V.replicate (ln - lb) emptyLookCountVector] in
      (na, nb) 

data FieldLookCount =
    FieldLookCount !(V.Vector (VU.Vector ObservationCount))
  | NoFieldLookCount
  deriving (Eq, Show, Generic)

instance NFData FieldLookCount

data FieldNumericState =
    FieldNumericState !(V.Vector NumericState)
  | NoFieldNumericState
  deriving (Eq, Show, Generic)

instance NFData FieldNumericState

data SVParseState =
  SVParseState {
    _badRows :: {-# UNPACK #-} !RowCount
  , _totalRows :: {-# UNPACK #-} !RowCount
  , _numFields :: !(Set FieldCount)
  , _fieldLooks :: !FieldLookCount
  , _textCounts :: !TextCounts
  , _numericState :: !FieldNumericState
  } deriving (Eq, Show, Generic)

instance NFData SVParseState

makeLenses ''SVParseState

resolveSVParseState :: TextFreeformThreshold -> [SVParseState] -> SVParseState
resolveSVParseState fft = foldr (combineSVParseState fft) initialSVParseState

combineSVParseState :: TextFreeformThreshold -> SVParseState -> SVParseState -> SVParseState
combineSVParseState fft s !acc =
    (badRows %~ ((s ^. badRows) +))
  . (totalRows %~ ((s ^. totalRows) +))
  . (numFields %~ ((s ^. numFields) `S.union`))
  . (fieldLooks %~ ((s ^. fieldLooks) `combineFieldLooks`))
  . (textCounts %~ ((s ^. textCounts) `combineTextCounts'`))
  . (numericState %~ ((s ^. numericState) `combineNumericState`))
  $! acc
  where
    combineTextCounts' = combineTextCounts fft
{-# INLINE combineSVParseState #-}

-- | We don't include a ParsedText here; Text is indicated by failure of
-- the parser.
data ParsedField =
    ParsedIntegral
  | ParsedReal
  | ParsedBoolean
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData ParsedField

renderParsedField :: ParsedField
                  -> Text
renderParsedField = T.pack . show

initialSVParseState :: SVParseState
initialSVParseState =
  SVParseState 0 0 S.empty NoFieldLookCount NoTextCounts NoFieldNumericState
