{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Warden.Data.Chunk (
    Chunk(..)
  , ChunkSize(..)
  , ChunkOffset(..)
  , ChunkCount(..)
  ) where

import          P

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
    unChunkCount :: Integer
  } deriving (Eq, Show, Ord, Num)

data Chunk = Chunk !ChunkOffset !ChunkSize
