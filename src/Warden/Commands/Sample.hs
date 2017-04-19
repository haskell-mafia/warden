{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands.Sample (
    extractNumericFields
) where

import           Control.Monad.Trans.Resource (ResourceT)

import           System.IO (IO, FilePath)

import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT, left)

extractNumericFields
  :: FilePath
  -> [FilePath]
  -> EitherT WardenError (ResourceT IO) ()
extractNumericFields _outp _fs =
  left WardenNotImplementedError

