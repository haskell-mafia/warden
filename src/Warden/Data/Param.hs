{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.Data.Param (
    CheckParams(..)
  , ExitType(..)
  , Force(..)
  , IncludeDotFiles(..)
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
  ) where

import           P

import           Debruijn.Hex (Hex, renderHex, parseHex)

import           GHC.Generics (Generic)

import           Warden.Data.Chunk
import           Warden.Data.Row
import           Warden.Data.Schema
import           Warden.Data.TextCounts

-- | 'RunId' is unique across invocations of warden, and can be used to 
-- correlate file markers with the associated view marker from the run.
newtype RunId =
  RunId {
    unRunId :: Hex
  } deriving (Eq, Show, Generic)

instance NFData RunId

renderRunId :: RunId -> Text
renderRunId = renderHex . unRunId

parseRunId :: Text -> Maybe RunId
parseRunId = fmap RunId . parseHex

newtype WardenVersion =
  WardenVersion {
    unWardenVersion :: Text
  } deriving (Eq, Show, Ord, Generic)

instance NFData WardenVersion

newtype NumCPUs =
  NumCPUs {
    unNumCPUs :: Int
  } deriving (Eq, Show, Generic)

instance NFData NumCPUs

chunksForCPUs :: NumCPUs -> ChunkCount
chunksForCPUs = ChunkCount . unNumCPUs

data Verbosity =
    Verbose
  | Quiet
  deriving (Eq, Show, Generic)

instance NFData Verbosity

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
  deriving (Eq, Show, Generic)

instance NFData Force

data ExitType =
    ExitWithCheckStatus
  | ExitWithSuccess
  deriving (Eq, Show, Generic)

instance NFData ExitType

data IncludeDotFiles =
    NoIncludeDotFiles
  | IncludeDotFiles
  deriving (Eq, Show, Generic)

instance NFData IncludeDotFiles

data CheckParams =
  CheckParams {
      checkSeparator :: !Separator
    , checkSchemaFile :: !(Maybe SchemaFile)
    , checkLineBound :: !LineBound
    , checkVerbosity :: !Verbosity
    , checkForce :: !Force
    , checkFreeformThreshold :: !TextFreeformThreshold
    , checkExitType :: !ExitType
    , checkIncludeDotFiles :: !IncludeDotFiles
    } deriving (Eq, Show, Generic)

instance NFData CheckParams

data SanityParams =
  SanityParams {
      sanityVerbosity :: !Verbosity
    , sanityForce :: !Force
    , sanityExitType :: !ExitType
    , sanityIncludeDotFiles :: !IncludeDotFiles
  } deriving (Eq, Show, Generic)

instance NFData SanityParams

data WardenParams =
  WardenParams {
      wpCaps :: NumCPUs
    , wpWardenVersion :: WardenVersion
    , wpRunId :: RunId
  } deriving (Eq, Show, Generic)

instance NFData WardenParams
