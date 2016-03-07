{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.Param (
    CheckParams(..)
  , Force(..)
  , NumCPUs(..)
  , WardenVersion(..)
  , chunksForCPUs
  ) where

import           P

import           Data.Text (Text)

import           Warden.Data.Check
import           Warden.Data.Chunk
import           Warden.Data.Row
import           Warden.Data.Schema

newtype WardenVersion =
  WardenVersion {
    unWardenVersion :: Text
  } deriving (Eq, Show, Ord)

newtype NumCPUs =
  NumCPUs {
    unNumCPUs :: Int
  } deriving (Eq, Show)

chunksForCPUs :: NumCPUs -> ChunkCount
chunksForCPUs = ChunkCount . unNumCPUs

data Force =
    Force
  | NoForce
  deriving (Eq, Show)

data CheckParams =
  CheckParams {
      checkSeparator :: !Separator
    , checkSchemaFile :: !(Maybe SchemaFile)
    , checkLineBound :: !LineBound
    , checkVerbosity :: !Verbosity
    , checkForce :: !Force
    } deriving (Eq, Show)
