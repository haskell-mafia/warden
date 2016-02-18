{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Check.File (
    runFileCheck
  , fileChecks
  , sanity
  , typeC
  , sizeC
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           P

import           System.IO (IO)

import           System.Posix.Files (isRegularFile, getSymbolicLinkStatus, fileSize)
import           System.Posix.Files (FileStatus)

import           Warden.Data
import           Warden.Debug
import           Warden.Error
import           Warden.Marker

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

runFileCheck :: Verbosity -> ViewFile -> FileCheck -> EitherT WardenError (ResourceT IO) CheckResult
runFileCheck verb f (FileCheck desc chk) = do
  liftIO . debugPrintLn verb $ T.concat [
      "Running file check "
    , renderCheckDescription desc
    , " on view file "
    , renderViewFile f
    , "."
    ]
  r <- chk f
  buildFileMarker f desc r >>= writeFileMarker
  pure $ FileCheckResult desc f r

buildFileMarker :: ViewFile -> CheckDescription -> CheckStatus -> EitherT WardenError (ResourceT IO) FileMarker
buildFileMarker vf cd cs = do
  t <- liftIO utcNow
  let mark = mkFileMarker vf cd t cs
  existsP <- liftIO $ fileMarkerExists vf
  if existsP
    then do
      old <- readFileMarker vf
      hoistEither $ combineFileMarker mark old
    else
      pure mark

fileChecks :: NonEmpty FileCheck
fileChecks = NE.fromList [
    FileCheck FileSanityChecks sanity
  ]

sanity :: ViewFile -> EitherT WardenError (ResourceT IO) CheckStatus
sanity (ViewFile fn) = do
  st <- liftIO $ getSymbolicLinkStatus fn
  pure . resolveCheckStatus $ typeC st <| pure (sizeC st)

typeC :: FileStatus -> CheckStatus
typeC st
  | isRegularFile st = CheckPassed
  | otherwise        = CheckFailed . pure $ SanityCheckFailure IrregularFile

sizeC :: FileStatus -> CheckStatus
sizeC st
  | fileSize st > 0 = CheckPassed
  | otherwise       = CheckFailed . pure $ SanityCheckFailure EmptyFile
