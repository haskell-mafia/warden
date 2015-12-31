{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Check (
    checks
  , runCheck
  ) where

import           Data.List.NonEmpty (NonEmpty(..))

import           P

import           System.IO (IO)

import qualified Warden.Check.File as File
import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT)

runCheck :: ViewFile -> WardenCheck -> EitherT WardenError IO CheckResult
runCheck f (WardenFileCheck fc) = File.runFileCheck fc f

checks :: NonEmpty WardenCheck
checks =
     WardenFileCheck <$> File.fileChecks
