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
import           Warden.View

import           X.Control.Monad.Trans.Either (EitherT)

check :: NumCPUs -> View -> CheckParams -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
check caps v ps =
  traverseView v >>= (checkViewFiles caps ps v)

fileCheck :: NumCPUs -> ViewFile -> CheckParams -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
fileCheck caps vf ps = do
  vf' <- liftIO . makeAbsolute $ unViewFile vf
  let view = View $ takeDirectory vf'
  checkViewFiles caps ps view $ vf :| []

checkViewFiles :: NumCPUs -> CheckParams -> View -> NonEmpty ViewFile -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
checkViewFiles caps (CheckParams s lb verb) v vfs = do
  frs <- fmap join $ traverse (forM File.fileChecks) $ (File.runFileCheck verb) <$> vfs
  rr <- Row.runRowCheck caps verb s v lb vfs
  pure $ rr <| frs

