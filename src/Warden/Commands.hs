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

check :: WardenVersion
      -> NumCPUs
      -> View
      -> CheckParams
      -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
check wv caps v ps =
  traverseView v >>= (checkViewFiles wv caps ps v)

fileCheck :: WardenVersion
          -> NumCPUs
          -> ViewFile
          -> CheckParams
          -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
fileCheck wv caps vf ps = do
  vf' <- liftIO . makeAbsolute $ viewFilePath vf
  let view = View $ takeDirectory vf'
  checkViewFiles wv caps ps view $ vf :| []

checkViewFiles :: WardenVersion
               -> NumCPUs
               -> CheckParams
               -> View
               -> NonEmpty ViewFile
               -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
checkViewFiles wv caps ps@(CheckParams _s sf _lb verb _fce) v vfs = do
  schema <- maybe (pure Nothing) (fmap Just . readSchema) sf
  frs <- fmap join $ traverse (forM File.fileChecks) $ (File.runFileCheck wv verb) <$> vfs
  rr <- Row.runRowCheck wv caps ps schema v vfs
  pure $ rr <| frs

