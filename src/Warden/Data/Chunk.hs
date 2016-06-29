{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Data.Chunk (
    Chunk(..)
  , ChunkSize(..)
  , ChunkOffset(..)
  , ChunkCount(..)
  ) where

import           P

newtype ChunkSize =
  ChunkSize {
    unChunkSize :: Integer
  } deriving (Eq, Show, Ord, Num)

newtype ChunkOffset =
  ChunkOffset {
    unChunkOffset :: Integer
  } deriving (Eq, Show, Ord, Num)

newtype ChunkCount =
  ChunkCount {
    unChunkCount :: Int
  } deriving (Eq, Show, Ord, Num)

data Chunk = Chunk !ChunkOffset !ChunkSize
  deriving (Eq, Show)
