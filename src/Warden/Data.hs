{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data (
    CheckParams(..)

  , module X
  ) where

import           P

import           Warden.Data.Check as X
import           Warden.Data.Marker as X
import           Warden.Data.Numeric as X
import           Warden.Data.Row as X
import           Warden.Data.View as X

data CheckParams =
  CheckParams View Separator LineBound
  deriving (Eq, Show)
