{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- This module will probably end up in brandix. -}

module Warden.Data.Schema (
    FieldForm(..)
  , FieldUniques(..)
  , Schema(..)
  , SchemaField(..)
  , SchemaFile(..)
  , SchemaVersion(..)
  , currentSchemaVersion
  , renderSchemaFile
  ) where

import qualified Data.Text as T
import           Data.Vector (Vector)

import           P

import           System.IO (FilePath)

import           Warden.Data.Field

newtype SchemaFile =
  SchemaFile {
      unSchemaFile :: FilePath
  } deriving (Eq, Show)

renderSchemaFile :: SchemaFile -> Text
renderSchemaFile = T.pack . unSchemaFile

newtype FieldUniques =
  FieldUniques {
    unFieldUniques :: Int
  } deriving (Eq, Show)

data FieldForm =
    FreeForm
  | CategoricalForm !FieldUniques
  | UnknownForm
  deriving (Eq, Show)

data SchemaVersion =
    SchemaV1
  deriving (Eq, Show, Enum, Bounded)

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = maxBound

data Schema = Schema !SchemaVersion !(Vector SchemaField)
  deriving (Eq, Show)

-- | This will contain more data in the future, e.g., summary statistics for
-- numeric types.
data SchemaField = SchemaField {
      schemaFieldType :: !FieldType
    , schemaFieldForm :: !FieldForm
  } deriving (Eq, Show)
