{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Warden.Data.Schema (
    FieldType(..)
  , Schema(..)
  , SchemaField(..)
  ) where

import          Data.Vector (Vector)

import          P

import          Warden.Data.Row

data Schema = Schema !FieldCount !(Vector SchemaField)
  deriving (Eq, Show)

-- | This will contain more data in the future, e.g., summary statistics for
-- numeric types.
data SchemaField = SchemaField !FieldType
  deriving (Eq, Show)

data FieldType =
    TextField
  | CategoricalField
  | IntegralField
  | RealField
  deriving (Eq, Show, Enum, Bounded)
