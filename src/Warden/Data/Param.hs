{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Data.Param (
    CheckParams(..)
  , ExitType(..)
  , Force(..)
  , IncludeDotFiles(..)
  , InferUsingFailedChecks(..)
  , NumCPUs(..)
  , PIICheckType(..)
  , RunId(..)
  , SamplingType(..)
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

import           Control.DeepSeq.Generics (genericRnf)

import           P

import           Debruijn.Hex (Hex, renderHex, parseHex)

import           GHC.Generics (Generic)

import           Warden.Data.Chunk
import           Warden.Data.PII
import           Warden.Data.Row
import           Warden.Data.Sampling.Reservoir
import           Warden.Data.Schema
import           Warden.Data.TextCounts

-- | 'RunId' is unique across invocations of warden, and can be used to 
-- correlate file markers with the associated view marker from the run.
newtype RunId =
  RunId {
    unRunId :: Hex
  } deriving (Eq, Show, Generic)

instance NFData RunId where rnf = genericRnf

renderRunId :: RunId -> Text
renderRunId = renderHex . unRunId

parseRunId :: Text -> Maybe RunId
parseRunId = fmap RunId . parseHex

newtype WardenVersion =
  WardenVersion {
    unWardenVersion :: Text
  } deriving (Eq, Show, Ord, Generic)

instance NFData WardenVersion where rnf = genericRnf

newtype NumCPUs =
  NumCPUs {
    unNumCPUs :: Int
  } deriving (Eq, Show, Generic)

instance NFData NumCPUs where rnf = genericRnf

chunksForCPUs :: NumCPUs -> ChunkCount
chunksForCPUs = ChunkCount . unNumCPUs

data Verbosity =
    Verbose
  | Quiet
  deriving (Eq, Show, Generic)

instance NFData Verbosity where rnf = genericRnf

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

instance NFData Force where rnf = genericRnf

data ExitType =
    ExitWithCheckStatus
  | ExitWithSuccess
  deriving (Eq, Show, Generic)

instance NFData ExitType where rnf = genericRnf

data IncludeDotFiles =
    NoIncludeDotFiles
  | IncludeDotFiles
  deriving (Eq, Show, Generic)

instance NFData IncludeDotFiles where rnf = genericRnf

data SamplingType =
    NoSampling
  | ReservoirSampling !ReservoirSize
  deriving (Eq, Show, Generic)

instance NFData SamplingType where rnf = genericRnf

data PIICheckType =
    NoPIIChecks
  | PIIChecks !MaxPIIObservations
  deriving (Eq, Show, Generic)

instance NFData PIICheckType where rnf = genericRnf

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
    , checkSamplingType :: !SamplingType
    , checkFileFormat :: !FileFormat
    , checkPIICheckType :: !PIICheckType
    } deriving (Eq, Show, Generic)

instance NFData CheckParams where rnf = genericRnf

data SanityParams =
  SanityParams {
      sanityVerbosity :: !Verbosity
    , sanityForce :: !Force
    , sanityExitType :: !ExitType
    , sanityIncludeDotFiles :: !IncludeDotFiles
  } deriving (Eq, Show, Generic)

instance NFData SanityParams where rnf = genericRnf

data WardenParams =
  WardenParams {
      wpCaps :: NumCPUs
    , wpWardenVersion :: WardenVersion
    , wpRunId :: RunId
  } deriving (Eq, Show, Generic)

data InferUsingFailedChecks =
    InferUsingFailedChecks
  | NoInferUsingFailedChecks
  deriving (Eq, Show)

instance NFData WardenParams where rnf = genericRnf
