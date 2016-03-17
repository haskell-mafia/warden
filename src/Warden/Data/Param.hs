{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.Param (
    CheckParams(..)
  , Force(..)
  , NumCPUs(..)
  , RunId(..)
  , SanityParams(..)
  , Verbosity(..)
  , WardenParams(..)
  , WardenVersion(..)
  , chunksForCPUs
  , parseRunId
  , parseVerbosity
  , renderRunId
  , renderVerbosity
  , runIdLength
  ) where

import           P

import           Debruijn.Hex (Hex, unHex, parseHex)

import           Warden.Data.Chunk
import           Warden.Data.Row
import           Warden.Data.Schema
import           Warden.Data.TextCounts

-- | 'RunId' is unique across invocations of warden, and can be used to 
-- correlate file markers with the associated view marker from the run.
newtype RunId =
  RunId {
    unRunId :: Hex
  } deriving (Eq, Show)

runIdLength :: Int
runIdLength = 16

renderRunId :: RunId -> Text
renderRunId = unHex . unRunId

parseRunId :: Text -> Maybe RunId
parseRunId t = case parseHex runIdLength t of
  Left _ -> Nothing
  Right h -> pure $ RunId h

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


data Verbosity =
    Verbose
  | Quiet
  deriving (Eq, Show)

renderVerbosity :: Verbosity -> Text
renderVerbosity Verbose = "verbose"
renderVerbosity Quiet = "quiet"

parseVerbosity :: Text -> Maybe Verbosity
parseVerbosity "verbose" = pure Verbose
parseVerbosity "quiet" = pure Quiet
parseVerbosity _ = Nothing

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
    , checkFreeformThreshold :: !TextFreeformThreshold
    } deriving (Eq, Show)

data SanityParams =
  SanityParams {
      sanityVerbosity :: !Verbosity
    , sanityForce :: !Force
  } deriving (Eq, Show)

data WardenParams =
  WardenParams {
      wpCaps :: NumCPUs
    , wpWardenVersion :: WardenVersion
    , wpRunId :: RunId
  } deriving (Eq, Show)
