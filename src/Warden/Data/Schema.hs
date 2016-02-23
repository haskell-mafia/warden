{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Warden.Data.Schema (
    SchemaField(..)
  ) where

import          Data.Vector (Vector)

import          P

import          Warden.Data.Row

data Schema = Schema !FieldCount !(Vector SchemaField)
  deriving (Eq, Show)

data SchemaField =
    TextField
  | CategoricalField
  | IntegralField
  | RealField
  deriving (Eq, Show, Enum, Bounded)
