{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Check (
    checks
  ) where

import           Data.List.NonEmpty (NonEmpty(..), (<|))

import           P

import qualified Warden.Check.File as File
import qualified Warden.Check.Row as Row
import           Warden.Data

checks :: NonEmpty WardenCheck
checks =
     (WardenRowCheck Row.rowCountsCheck)
  <| (WardenFileCheck <$> File.fileChecks)
