{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Check (
    checks
  ) where

import           P

import           Warden.Data
import qualified Warden.Check.File as File

checks :: [WardenCheck]
checks = concat [
    WardenFileCheck <$> File.fileChecks
  ]
