{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Warden.Data (
    Row(..)
  , WardenStatus(..)
  , CheckResult(..)
  , SVParseState(..)
  , Minimum(..)
  , Maximum(..)
  , Mean(..)
  , Median(..)
  , Variance(..)
  , NumericSummary(..)
  , RowSchema(..)
  , WardenCheck(..)
  , inferFields
  ) where

import           P

import           Data.Map     (Map)
import           Data.Text    (Text)
import           Data.Vector  (Vector)
import           Pipes

-- | Raw record. Can be extended to support JSON objects as well as xSV if
--   needed.
data Row = SVFields (Vector Text)
           | RowFailure Text
           | SVEOF
  deriving (Eq, Show)

data WardenStatus = Green
                    -- ^ No issues detected.
                  | Yellow
                    -- ^ Some values are concerning and should be investigated
                    --   by a human.
                  | Red
                    -- ^ At least one check failed, processing should not
                    --   proceed without human intervention.
                  | Unknown
                    -- ^ We broke, or weren't given enough data.
  deriving (Eq, Show, Ord)

-- FIXME(sharif): this could use more structure once we have a better idea of
--                what failures look like - row(s) affected, et cetera
data CheckResult = CheckResult WardenStatus Text

data RowSchema a = RowSchema
  { fromRow :: Row -> Maybe a }

data WardenCheck a b = WardenCheck
  { initial  :: a
  , update   :: a -> RowSchema b -> a
  , finalize :: a -> CheckResult
  }

newtype Minimum = Minimum { getMininum :: Double }
  deriving (Eq, Show)

newtype Maximum = Maximum { getMaximum :: Double }
  deriving (Eq, Show)

newtype Mean = Mean { getMean :: Double }
  deriving (Eq, Show)

newtype Median = Median { getMedian :: Double }
  deriving (Eq, Show)

newtype Variance = Variance { getVariance :: Double }
  deriving (Eq, Show)

-- | So we can cheaply keep track of long-term change in numeric datasets.
--   Will probably also end up in brandix.

-- NB(sharif): I'm not sure if the median is worth calculating as it
--             can't be done cheaply, but it's handy for tests like
--             S-H-ESD; should rethink once we know more about which
--             tests actually work.
data NumericSummary = NumericSummary
  { _min    :: Minimum
  , _max    :: Maximum
  , _mean   :: Mean
  , _var    :: Maybe Variance
  , _median :: Maybe Median
  }
  deriving (Eq, Show)

-- | We try parsing a field as each of these in order until we find one that
--   works.
data FieldLooks = LooksEmpty
                | LooksIntegral
                | LooksReal
                | LooksText
  deriving (Eq, Show, Ord)

-- | We keep track of the number of unique values we get in each field; if
--   it's greater than a certain percentage of the total number of records
--   it's probably freeform, otherwise it's probably categorical. We
--   make the distinction here so we don't have to drag around every
--   freeform record in memory.
data TextCount = TextCount     (Map Text Integer)
               | LooksFreeform
  deriving (Eq, Show)

data SVParseState = SVParseState
  { _badRecords   :: Integer
  , _totalRecords :: Integer
  , _numFields    :: Integer
  , _fieldCounts  :: Vector (Map FieldLooks Integer, TextCount)
  } deriving (Eq, Show)

inferFields :: (Monad m)
            => Producer Row m ()
            -> m SVParseState
inferFields = fail "nyi"

