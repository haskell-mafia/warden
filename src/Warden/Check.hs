{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Check (
    checks
  , runCheck
  ) where

import           P

import           System.IO (IO)

import qualified Warden.Check.File as File
import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT)

runCheck :: ViewFile -> WardenCheck -> EitherT WardenError IO CheckResult
runCheck f (WardenFileCheck fc) = File.runFileCheck fc f

checks :: [WardenCheck]
checks = concat [
    WardenFileCheck <$> File.fileChecks
  ]
