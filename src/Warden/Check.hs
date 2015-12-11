{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Check (
    sanity
  , typeC
  , sizeC
  ) where

import           Control.Monad.IO.Class (liftIO)

import           P

import           System.Posix.Files (isRegularFile, getSymbolicLinkStatus, fileSize)
import           System.Posix.Files (FileStatus)

import           Warden.Data

sanity :: FileCheck
sanity fn = do
  st <- liftIO $ getSymbolicLinkStatus fn
  pure $ [typeC st, sizeC st]

typeC :: FileStatus -> CheckStatus
typeC st
  | isRegularFile st = CheckPassed
  | otherwise        = CheckFailed $ SanityCheckFailure IrregularFile

sizeC :: FileStatus -> CheckStatus
sizeC st
  | fileSize st > 0 = CheckPassed
  | otherwise       = CheckFailed $ SanityCheckFailure EmptyFile
