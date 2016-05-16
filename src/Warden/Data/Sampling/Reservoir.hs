{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Warden.Data.Sampling.Reservoir(
    Reservoir(..)
  , ReservoirAcc(..)
  , ReservoirSize(..)
  , SampleCount(..)
  , initialSampleCount
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import qualified Data.Vector.Unboxed.Mutable as MVU

import           GHC.Generics (Generic)

import           P

data ReservoirAcc =
    NoReservoirAcc
  | ReservoirAcc {-# UNPACK #-} !Reservoir {-# UNPACK #-} !SampleCount
  deriving Generic

instance NFData ReservoirAcc where rnf = genericRnf

newtype Reservoir =
  Reservoir {
    unReservoir :: MVU.IOVector Double
  } deriving Generic

instance NFData Reservoir where rnf = genericRnf

newtype ReservoirSize =
  ReservoirSize {
    unReservoirSize :: Int
  } deriving (Eq, Show, Generic, Ord, Num)

instance NFData ReservoirSize where rnf = genericRnf

newtype SampleCount =
  SampleCount {
    unSampleCount :: Int
  } deriving (Eq, Show, Generic, Ord, Num)

instance NFData SampleCount where rnf = genericRnf

initialSampleCount :: SampleCount
initialSampleCount = SampleCount 0
