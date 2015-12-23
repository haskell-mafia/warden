{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
  check
) where

import           P

import           System.IO (IO)

import           Warden.Check
import           Warden.Data
import           Warden.Error
import           Warden.View

import           X.Control.Monad.Trans.Either (EitherT)

-- FIXME: do something more useful with check results
check :: View -> EitherT WardenError IO [CheckResult]
check v = do
  vfs <- traverseView v
  fmap concat $ mapM (\r -> mapM r checks) $ runCheck <$> vfs

