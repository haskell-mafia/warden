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
  , LineBound(..)
  , ObservationCount(..)
  , ParsedField(..)
  , Row(..)
  , RowCount(..)
  , SVParseState(..)
  , Separator(..)
  , asciiToLower
  , badRows
  , combineFieldLooks
  , fieldP
  , fieldLooks
  , initialSVParseState
  , numFields
  , parseField
  , renderFieldCount
  , renderParsedField
  , resolveSVParseState
  , totalRows
  , updateFieldLooks
  , updateSVParseState
) where

import           Control.Lens

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char (chr, ord)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Word (Word8)

import           GHC.Generics (Generic)

import           P

import           Prelude (fromEnum)

import           Warden.Data.Field

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
renderFieldCount = T.pack . show . unFieldCount

newtype ObservationCount =
  ObservationCount {
    unObservationCount :: Int64
  } deriving (Eq, Show, Ord, Num, Generic)

$(derivingUnbox "ObservationCount"
  [t| ObservationCount -> Int64 |]
  [| \(ObservationCount x) -> x |]
  [| \x -> (ObservationCount x) |])

instance NFData ObservationCount

newtype Separator =
  Separator {
    unSeparator :: Word8
  } deriving (Eq, Show, Generic)

instance NFData Separator

-- | Raw record. Can be extended to support JSON objects as well as xSV if
--   needed.
data Row =
    SVFields !(V.Vector Text)
  | RowFailure !Text
  deriving (Eq, Show, Generic)

instance NFData Row

newtype RowCount =
  RowCount {
    unRowCount :: Int64
  } deriving (Eq, Show, Ord, Num, Generic)

instance NFData RowCount

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
  deriving (Eq, Show)

data SVParseState =
  SVParseState {
    _badRows     :: {-# UNPACK #-} !RowCount
  , _totalRows   :: {-# UNPACK #-} !RowCount
  , _numFields   :: !(Set FieldCount)
  , _fieldLooks  :: !FieldLookCount
  } deriving (Eq, Show, Generic)

instance NFData SVParseState

makeLenses ''SVParseState

resolveSVParseState :: [SVParseState] -> SVParseState
resolveSVParseState = foldr update initialSVParseState
  where
    update s !acc =
        (badRows %~ ((s ^. badRows) +))
      . (totalRows %~ ((s ^. totalRows) +))
      . (numFields %~ ((s ^. numFields) `S.union`))
      . (fieldLooks %~ ((s ^. fieldLooks) `combineFieldLooks`))
      $! acc

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

-- | We only care about ASCII characters here (true, false et cetera)
-- and converting unicode to lowercase is really expensive, so just
-- add 32 to the character if it's in the ASCII uppercase range.
asciiToLower :: Text -> Text
asciiToLower = T.map charToLower
  where
    charToLower c
      | c >= 'A' && c <= 'Z' = chr $ ord c + 0x20
      | otherwise            = c
{-# INLINE asciiToLower #-}

parseField :: Text -> FieldLooks
parseField "" = LooksEmpty
parseField t = case parseOnly fieldP (asciiToLower t) of
  Left _ -> LooksText
  Right ParsedIntegral -> LooksIntegral
  Right ParsedReal -> LooksReal
  Right ParsedBoolean -> LooksBoolean
{-# INLINE parseField #-}

updateFieldLooks :: Text -> VU.Vector ObservationCount -> VU.Vector ObservationCount
updateFieldLooks !t !a =
  VU.accum (+) a [(fromEnum (parseField t), 1)]
{-# INLINE updateFieldLooks #-}

fieldP :: Parser ParsedField
fieldP = choice [
    void (signed (decimal :: Parser Integer) <* endOfInput) >> pure ParsedIntegral
  , void (double <* endOfInput) >> pure ParsedReal
  , void (boolP <* endOfInput) >> pure ParsedBoolean
  ]
{-# INLINE fieldP #-}

boolP :: Parser ()
boolP = trueP <|> falseP
  where
    trueP = do
      void $ char 't'
      peekChar >>= \case
        Nothing -> pure ()
        Just _ -> void $ string "rue"

    falseP = do
      void $ char 'f'
      peekChar >>= \case
        Nothing -> pure ()
        Just _ -> void $ string "alse"
{-# INLINE boolP #-}

initialSVParseState :: SVParseState
initialSVParseState = SVParseState 0 0 S.empty NoFieldLookCount

-- | Accumulator for field/row counts on tokenized raw data.
updateSVParseState :: SVParseState
                   -> Row
                   -> SVParseState
updateSVParseState !st row =
  let good = countGood row
      bad  = countBad row  in
    (totalRows %~ ((good + bad) +))
  . (badRows %~ (bad +))
  . (numFields %~ (updateNumFields row))
  . (fieldLooks %~ (updateFields row))
  $!! st
 where
  countGood (SVFields _)   = RowCount 1
  countGood (RowFailure _) = RowCount 0

  countBad (SVFields _)    = RowCount 0
  countBad (RowFailure _)  = RowCount 1

  updateNumFields (SVFields !v) !ns =
    let n = FieldCount $ V.length v in
    S.insert n ns
  updateNumFields _ !ns = ns

  updateFields (SVFields !v) NoFieldLookCount =
    FieldLookCount $ V.zipWith updateFieldLooks v $
      V.replicate (V.length v) emptyLookCountVector
  updateFields (SVFields !v) (FieldLookCount !a) =
    FieldLookCount $!! V.zipWith updateFieldLooks v a
  updateFields _ !a = a
{-# INLINE updateSVParseState #-}
