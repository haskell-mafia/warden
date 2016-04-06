{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Param(
    buildWardenParams
  ) where

import           Control.Concurrent (getNumCapabilities)

import           Debruijn.Hex (genHex)

import           P

import           System.IO (IO)
import           System.Random.MWC (withSystemRandom)

import           Warden.Data.Param

buildWardenParams :: WardenVersion -> IO WardenParams
buildWardenParams v = WardenParams <$> getNumCPUs
                                   <*> pure v
                                   <*> genRunId

genRunId :: IO RunId
genRunId = RunId <$> (withSystemRandom genHex)

getNumCPUs :: IO NumCPUs
getNumCPUs = NumCPUs <$> getNumCapabilities
