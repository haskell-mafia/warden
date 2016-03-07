{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Param(
    genRunId
  , getNumCPUs
  ) where

import           Control.Concurrent (getNumCapabilities)

import           Data.UUID.V4 (nextRandom)

import           P

import           System.IO (IO)

import           Warden.Data.Param

genRunId :: IO RunId
genRunId = RunId <$> nextRandom

getNumCPUs :: IO NumCPUs
getNumCPUs = NumCPUs <$> getNumCapabilities
