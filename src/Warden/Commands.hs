{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
  check
) where

import           Control.Monad.Trans.Resource (runResourceT)

import           Data.List.NonEmpty (NonEmpty, (<|))

import           P

import           System.IO (IO)

import qualified Warden.Check.File as File
import qualified Warden.Check.Row as Row

import           Warden.Data
import           Warden.Error
import           Warden.View

import           X.Control.Monad.Trans.Either (EitherT, mapEitherT)

check :: CheckParams -> EitherT WardenError IO (NonEmpty CheckResult)
check (CheckParams v s lb) = do
  vfs <- traverseView v
  frs <- fmap join $ traverse (forM File.fileChecks) $ File.runFileCheck <$> vfs
  rr <- mapEitherT runResourceT $ Row.runRowCheck s lb vfs
  pure $ rr <| frs

