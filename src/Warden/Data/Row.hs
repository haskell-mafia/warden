{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Data.Array (Array, accum, array, assocs)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char (chr, ord)
import           Data.List (repeat, zip)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word (Word8)

import           GHC.Generics (Generic)

import           P

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
    unObservationCount :: Integer
  } deriving (Eq, Show, Ord, Num, Generic)

instance NFData ObservationCount

newtype Separator =
  Separator {
    unSeparator :: Word8
  } deriving (Eq, Show, Generic)

instance NFData Separator

-- | Raw record. Can be extended to support JSON objects as well as xSV if
--   needed.
data Row =
    SVFields !(Vector Text)
  | RowFailure !Text
  | SVEOF
  deriving (Eq, Show, Generic)

instance NFData Row

newtype RowCount =
  RowCount {
    unRowCount :: Int
  } deriving (Eq, Show, Ord, Num, Generic)

instance NFData RowCount

emptyLookCountArray :: Array FieldLooks ObservationCount
emptyLookCountArray =
  array (minBound, maxBound) (zip [minBound..maxBound] $ repeat (ObservationCount 0))

combineFieldLooks :: FieldLookCount
                  -> FieldLookCount
                  -> FieldLookCount
combineFieldLooks NoFieldLookCount NoFieldLookCount = NoFieldLookCount
combineFieldLooks (FieldLookCount !x) NoFieldLookCount = FieldLookCount x
combineFieldLooks NoFieldLookCount (FieldLookCount !y) = FieldLookCount y
combineFieldLooks (FieldLookCount !x) (FieldLookCount !y) = FieldLookCount . uncurry combine' $ matchSize x y
  where
    combine' = V.zipWith addLooks

    addLooks a b = accum (+) a $ assocs b

    -- To retain some sanity in the event of mismatched field counts.
    matchSize a b =
      let la = V.length a
          lb = V.length b
          ln = max la lb
          na = V.concat [a, V.replicate (ln - la) emptyLookCountArray]
          nb = V.concat [b, V.replicate (ln - lb) emptyLookCountArray] in
      (na, nb) 

data FieldLookCount =
    FieldLookCount !(Vector (Array FieldLooks ObservationCount))
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

data ParsedField =
    ParsedIntegral
  | ParsedReal
  | ParsedText
  | ParsedBoolean
  deriving (Eq, Show, Generic)

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
  Left _ -> LooksBroken
  Right ParsedIntegral -> LooksIntegral
  Right ParsedReal -> LooksReal
  Right ParsedText -> LooksText
  Right ParsedBoolean -> LooksBoolean
{-# INLINE parseField #-}

updateFieldLooks :: Text -> Array FieldLooks ObservationCount -> Array FieldLooks ObservationCount
updateFieldLooks !t !a =
  accum  (+) a [(parseField t, 1)]
{-# INLINE updateFieldLooks #-}

fieldP :: Parser ParsedField
fieldP = choice [
    void (signed (decimal :: Parser Integer) <* endOfInput) >> pure ParsedIntegral
  , void (double <* endOfInput) >> pure ParsedReal
  , void (boolP <* endOfInput) >> pure ParsedBoolean
  , void takeText >> pure ParsedText
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
  countGood SVEOF          = RowCount 0

  countBad (SVFields _)    = RowCount 0
  countBad (RowFailure _)  = RowCount 1
  countBad SVEOF           = RowCount 0

  updateNumFields (SVFields !v) !ns =
    let n = FieldCount $ V.length v in
    S.insert n ns
  updateNumFields _ !ns = ns

  updateFields (SVFields !v) NoFieldLookCount =
    FieldLookCount $ V.zipWith updateFieldLooks v $
      V.replicate (V.length v) emptyLookCountArray
  updateFields (SVFields !v) (FieldLookCount !a) =
    FieldLookCount $!! V.zipWith updateFieldLooks v a
  updateFields _ !a = a
