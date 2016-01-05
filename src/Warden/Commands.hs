{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
  check
) where

import           Data.List.NonEmpty (NonEmpty, (<|))

import           P

import           System.IO (IO)

import qualified Warden.Check.File as File
import qualified Warden.Check.Row as Row

import           Warden.Data
import           Warden.Error
import           Warden.View

import           X.Control.Monad.Trans.Either (EitherT)

-- FIXME: do something more useful with check results
check :: CheckParams -> EitherT WardenError IO (NonEmpty CheckResult)
check (CheckParams v s) = do
  vfs <- traverseView v
  frs <- fmap join $ traverse (forM File.fileChecks) $ File.runFileCheck <$> vfs
  rr <- Row.runRowCheck s vfs Row.rowCountsCheck
  pure $ rr <| frs

