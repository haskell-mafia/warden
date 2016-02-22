{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
  check
) where

import           Control.Monad.Trans.Resource (ResourceT)

import           Data.List.NonEmpty (NonEmpty, (<|))

import           P

import           System.IO (IO)

import qualified Warden.Check.File as File
import qualified Warden.Check.Row as Row

import           Warden.Data
import           Warden.Error
import           Warden.View

import           X.Control.Monad.Trans.Either (EitherT)

check :: NumCPUs -> View -> CheckParams -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
check caps v (CheckParams s lb verb) = do
  vfs <- traverseView v
  frs <- fmap join $ traverse (forM File.fileChecks) $ (File.runFileCheck verb) <$> vfs
  rr <- Row.runRowCheck caps verb s v lb vfs
  pure $ rr <| frs

