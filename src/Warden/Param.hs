{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Param(
    buildWardenParams
  ) where

import           Control.Concurrent (getNumCapabilities)

import           Debruijn.Hex (keyHex)

import           P

import           System.IO (IO)

import           Warden.Data.Param

buildWardenParams :: WardenVersion -> IO WardenParams
buildWardenParams v = WardenParams <$> getNumCPUs
                                   <*> pure v
                                   <*> genRunId

genRunId :: IO RunId
genRunId = RunId <$> keyHex runIdLength

getNumCPUs :: IO NumCPUs
getNumCPUs = NumCPUs <$> getNumCapabilities
