{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.Param (
    CheckParams(..)
  , Force(..)
  , NumCPUs(..)
  , RunId(..)
  , WardenParams(..)
  , WardenVersion(..)
  , chunksForCPUs
  ) where

import           P

import           Data.Text (Text)
import           Data.UUID (UUID)

import           Warden.Data.Check
import           Warden.Data.Chunk
import           Warden.Data.Row
import           Warden.Data.Schema

newtype RunId =
  RunId {
    unRunId :: UUID
  } deriving (Eq, Show)

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

data WardenParams =
  WardenParams {
      wpCaps :: NumCPUs
    , wpWardenVersion :: WardenVersion
    , wpRunId :: RunId
  } deriving (Eq, Show)
