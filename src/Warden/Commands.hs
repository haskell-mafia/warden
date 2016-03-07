{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
    check
  , fileCheck
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.List.NonEmpty (NonEmpty(..), (<|))

import           P

import           System.Directory (makeAbsolute)
import           System.FilePath (takeDirectory)
import           System.IO (IO)

import qualified Warden.Check.File as File
import qualified Warden.Check.Row as Row

import           Warden.Data
import           Warden.Error
import           Warden.Schema
import           Warden.View

import           X.Control.Monad.Trans.Either (EitherT)

check :: WardenParams
      -> View
      -> CheckParams
      -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
check wps v ps =
  traverseView v >>= (checkViewFiles wps ps v)

fileCheck :: WardenParams
          -> ViewFile
          -> CheckParams
          -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
fileCheck wps vf ps = do
  vf' <- liftIO . makeAbsolute $ viewFilePath vf
  let view = View $ takeDirectory vf'
  checkViewFiles wps ps view $ vf :| []

checkViewFiles :: WardenParams
               -> CheckParams
               -> View
               -> NonEmpty ViewFile
               -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
checkViewFiles wps ps@(CheckParams _s sf _lb verb _fce) v vfs = do
  schema <- maybe (pure Nothing) (fmap Just . readSchema) sf
  frs <- fmap join $ traverse (forM File.fileChecks) $ (File.runFileCheck wps verb) <$> vfs
  rr <- Row.runRowCheck wps ps schema v vfs
  pure $ rr <| frs

