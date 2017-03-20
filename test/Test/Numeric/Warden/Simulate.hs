{-# LANGUAGE NoImplicitPrelude #-}

module Test.Numeric.Warden.Simulate(
    Computation(..)
  ) where

import           P

data Computation =
    Summation
  deriving (Eq, Show, Ord, Bounded, Enum)
