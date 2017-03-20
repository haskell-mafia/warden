{-# LANGUAGE NoImplicitPrelude #-}

module Test.Numeric.Warden.Simulate(
    Computation(..)
  , TestRange(..)
  , fixedMean
  , welfordMean
  ) where

import           Data.FixedPoint (FixedPoint256256)
import           Data.Vector (Vector)
import qualified Data.Vector as VU

import           P

import           Warden.Data
import           Warden.Numeric

data Computation =
    DataMean
  deriving (Eq, Show, Ord, Bounded, Enum)

data TestRange =
  TestRange {
    testLower :: !Double
  , testUpper :: !Double
  } deriving (Eq, Show)

fixedMean :: Vector FixedPoint256256 -> Maybe' FixedPoint256256
fixedMean xv =
  let
    n = fromIntegral $ VU.length xv
  in
  case n of
    0 -> Nothing'
    _ -> Just' $ (VU.foldl' (+) 0 xv) / n

welfordMean :: Vector Double -> Maybe' Double
welfordMean xv =
  case fst . finalizeMeanDev $ VU.foldl' updateMeanDev MeanDevInitial xv of
    NoMean -> Nothing'
    Mean x -> pure x
