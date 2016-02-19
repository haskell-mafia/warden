{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Warden.Data.Row (
    FieldCount(..)
  , LineBound(..)
  , ParsedField(..)
  , Row(..)
  , RowCount(..)
  , SVParseState(..)
  , Separator(..)

  , field
  , renderParsedField
  , totalRows
  , badRows
  , numFields
  , initialSVParseState
  , updateSVParseState
  , resolveSVParseState
) where

import           Control.Lens

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.List (union)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word (Word8)

import           P

newtype FieldCount =
  FieldCount {
    unFieldCount :: Int
  } deriving (Eq, Show, Num)

newtype Separator = Separator { unSeparator :: Word8 }
  deriving (Eq, Show)

-- | Raw record. Can be extended to support JSON objects as well as xSV if
--   needed.
data Row = SVFields !(Vector Text)
           | RowFailure !Text
           | SVEOF
  deriving (Eq, Show)

newtype RowCount =
  RowCount {
    unRowCount :: Int
  } deriving (Eq, Show, Num)

data SVParseState = SVParseState
  { _badRows     :: {-# UNPACK #-} !RowCount
  , _totalRows   :: {-# UNPACK #-} !RowCount
  , _numFields   :: ![FieldCount]
  } deriving (Eq, Show)

makeLenses ''SVParseState

resolveSVParseState :: [SVParseState] -> SVParseState
resolveSVParseState = foldr update initialSVParseState
  where
    update s !acc =
        (badRows %~ ((s ^. badRows) +))
      . (totalRows %~ ((s ^. totalRows) +))
      . (numFields %~ ((s ^. numFields) `union`))
      $! acc

data ParsedField = IntegralField !Integer
                 | RealField !Double
                 | TextField !Text
  deriving (Eq, Show)

renderParsedField :: ParsedField
                  -> Text
renderParsedField (IntegralField i) = T.pack $ show i
renderParsedField (RealField d)     = T.pack $ show d
renderParsedField (TextField t)     = t

initialSVParseState :: SVParseState
initialSVParseState = SVParseState 0 0 []

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
  $! st
 where
  countGood (SVFields _)   = RowCount 1
  countGood (RowFailure _) = RowCount 0
  countGood SVEOF          = RowCount 0

  countBad (SVFields _)    = RowCount 0
  countBad (RowFailure _)  = RowCount 1
  countBad SVEOF           = RowCount 0

  updateNumFields (SVFields !v) !ns =
    let n = FieldCount $ V.length v in
    if not (elem n ns)
      then n : ns 
      else ns
  updateNumFields _ !ns = ns
{-# INLINE updateSVParseState #-}

field :: Parser ParsedField
field = choice
  [ IntegralField <$> signed decimal <* endOfInput
  , RealField     <$> double <* endOfInput
  , TextField     <$> takeText
  ]

newtype LineBound =
  LineBound {
    unLineBound :: Int
  } deriving (Eq, Show)
