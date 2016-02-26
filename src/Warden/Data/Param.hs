{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.Param (
    CheckParams(..)
  , Force(..)
  , NumCPUs(..)
  , chunksForCPUs
  ) where

import           P

import           Warden.Data.Check
import           Warden.Data.Chunk
import           Warden.Data.Row
import           Warden.Data.Schema

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
  CheckParams !Separator !(Maybe SchemaFile) !LineBound !Verbosity !Force
  deriving (Eq, Show)
