{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Data (
    CheckParams(..)

  , module X
  ) where

import           P

import           Warden.Data.Check as X
import           Warden.Data.Chunk as X
import           Warden.Data.Marker as X
import           Warden.Data.Numeric as X
import           Warden.Data.Row as X
import           Warden.Data.View as X

data CheckParams =
  CheckParams !View !Separator !LineBound !Verbosity
  deriving (Eq, Show)
