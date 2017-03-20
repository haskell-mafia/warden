{-# LANGUAGE NoImplicitPrelude #-}

module Test.Numeric.Warden.Simulate(
    Computation(..)
  , TestRange(..)
  ) where

import           P

data Computation =
    DataMean
  deriving (Eq, Show, Ord, Bounded, Enum)

data TestRange =
  TestRange {
    testLower :: !Double
  , testUpper :: !Double
  } deriving (Eq, Show)
