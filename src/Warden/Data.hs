{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Warden.Data (
    SVRow(..)
  , WardenStatus(..)
  , CheckResult(..)
  , SVParseState(..)
  , RowSchema
  , WardenCheck
  , fromRow
  , initial
  , update
  , finalize
  , inferFields

  , badRecords
  , totalRecords
  , numFields
  , fieldCounts
  ) where

import P

import Control.Lens
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Text (Text)
import Pipes

data SVRow = SVFields (Vector Text)
           | SVFailure Text
           | SVEOF
  deriving (Eq, Show)

data WardenStatus = Green
                   -- ^ No issues detected.
                 | Yellow
                   -- ^ Some values are concerning and should be investigated 
                   --   by a human.
                 | Red
                   -- ^ At least one check failed. 
                 | Unknown
                   -- ^ We broke, or weren't given enough data.

data CheckResult = CheckResult

class RowSchema a where
    fromRow :: SVRow -> Maybe a

class (RowSchema b) => WardenCheck a b where
    initial   :: a
    update    :: a -> b -> a
    finalize  :: a -> CheckResult

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

makeLenses ''SVParseState

inferFields :: (Monad m)
            => Producer SVRow m ()
            -> m SVParseState
inferFields = fail "nyi"

