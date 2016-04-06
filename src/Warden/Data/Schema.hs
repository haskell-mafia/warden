{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{- This module will probably end up in brandix. -}

module Warden.Data.Schema (
    FieldForm(..)
  , FieldUniques(..)
  , Schema(..)
  , SchemaField(..)
  , SchemaFile(..)
  , SchemaVersion(..)
  , currentSchemaVersion
  , renderFieldUniques
  , renderSchemaFile
  ) where

import qualified Data.Text as T
import           Data.Vector (Vector)

import           GHC.Generics (Generic)

import           P

import           System.IO (FilePath)

import           Warden.Data.Field

newtype SchemaFile =
  SchemaFile {
      unSchemaFile :: FilePath
  } deriving (Eq, Show, Generic)

instance NFData SchemaFile

renderSchemaFile :: SchemaFile -> Text
renderSchemaFile = T.pack . unSchemaFile

newtype FieldUniques =
  FieldUniques {
    unFieldUniques :: Int
  } deriving (Eq, Show, Ord, Generic)

instance NFData FieldUniques

renderFieldUniques :: FieldUniques -> Text
renderFieldUniques = renderIntegral . unFieldUniques

data FieldForm =
    FreeForm
  | CategoricalForm !FieldUniques
  deriving (Eq, Show, Generic)

instance NFData FieldForm

data SchemaVersion =
    SchemaV1
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData SchemaVersion

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = maxBound

data Schema = Schema !SchemaVersion !(Vector SchemaField)
  deriving (Eq, Show, Generic)

instance NFData Schema

-- | This will contain more data in the future, e.g., summary statistics for
-- numeric types.
data SchemaField = SchemaField {
      schemaFieldType :: !FieldType
    , schemaFieldForm :: !FieldForm
  } deriving (Eq, Show, Generic)

instance NFData SchemaField
