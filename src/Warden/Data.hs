{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data (
    CheckParams(..)
  , NumCPUs(..)
  , chunksForCPUs

  , module X
  ) where

import           P

import           Warden.Data.Check as X
import           Warden.Data.Chunk as X
import           Warden.Data.Marker as X
import           Warden.Data.Numeric as X
import           Warden.Data.Row as X
import           Warden.Data.View as X

newtype NumCPUs =
  NumCPUs {
    unNumCPUs :: Int
  } deriving (Eq, Show)

chunksForCPUs :: NumCPUs -> ChunkCount
chunksForCPUs = ChunkCount . unNumCPUs

data CheckParams =
  CheckParams !View !Separator !LineBound !Verbosity
  deriving (Eq, Show)
