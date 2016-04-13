{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Warden.Data.Sampling.Reservoir(
    ReservoirAcc(..)
  , ReservoirSize(..)
  , SampleCount(..)
  , initialSampleCount
  ) where

import qualified Data.Vector.Unboxed.Mutable as MVU

import           GHC.Generics (Generic)

import           P

newtype ReservoirAcc =
  ReservoirAcc {
    unReservoirAcc :: MVU.IOVector Double
  } deriving Generic

instance NFData ReservoirAcc

newtype ReservoirSize =
  ReservoirSize {
    unReservoirSize :: Int
  } deriving (Eq, Show, Generic, Ord, Num)

instance NFData ReservoirSize

newtype SampleCount =
  SampleCount {
    unSampleCount :: Int
  } deriving (Eq, Show, Generic, Ord, Num)

instance NFData SampleCount

initialSampleCount :: SampleCount
initialSampleCount = SampleCount 0
