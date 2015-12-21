{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
  check
) where

import           System.IO (IO)

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT, left)

check :: View -> EitherT WardenError IO CheckResult
check _ = left WardenNotImplementedError

