{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.Param (
    CheckParams(..)
  , Force(..)
  , NumCPUs(..)
  , RunId(..)
  , SanityParams(..)
  , WardenParams(..)
  , WardenVersion(..)
  , chunksForCPUs
  , parseRunId
  , renderRunId
  , runIdLength
  ) where

import           P

import           Data.Text (Text)

import           Debruijn.Hex (Hex, unHex, parseHex)

import           Warden.Data.Check
import           Warden.Data.Chunk
import           Warden.Data.Row
import           Warden.Data.Schema

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
