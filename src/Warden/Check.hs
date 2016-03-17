{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Check (
    FileCheck(..)
  ) where

import           Control.Monad.Trans.Resource (ResourceT)

import           System.IO (IO)

import           Warden.Data.Check
import           Warden.Data.View
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT)

data FileCheck =
    FileCheck !CheckDescription (ViewFile -> EitherT WardenError (ResourceT IO) CheckStatus)
