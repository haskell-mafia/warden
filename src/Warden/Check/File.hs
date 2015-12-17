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

import           Data.List.NonEmpty ((<|))

import           P

import           System.IO (IO)

import           System.Posix.Files (isRegularFile, getSymbolicLinkStatus, fileSize)
import           System.Posix.Files (FileStatus)

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT)

runFileCheck :: FileCheck -> ViewFile -> EitherT WardenError IO CheckResult
runFileCheck (FileCheck desc chk) f = do
  r <- chk f
  pure $ FileCheckResult desc f r

fileChecks :: [FileCheck]
fileChecks = [
    FileCheck (CheckDescription "basic sanity checks") sanity
  ]

sanity :: ViewFile -> EitherT WardenError IO CheckStatus
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
