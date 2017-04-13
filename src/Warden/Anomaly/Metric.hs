{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Anomaly.Metric (
    Distance(..)
  , distance0
  , euclidean
  ) where

import           Data.AEq (AEq)

import qualified Data.Vector.Algorithms.Insertion as Insertion
import qualified Data.Vector.Unboxed as VU

import           P

import           Warden.Anomaly.Data

-- | Distance in an arbitrary metric space (on R^n).
--
-- forall (f :: FeatureVector -> FeatureVector -> Distance)
--
--   f x y >= 0
--   (f x y == 0) ==> x == y
--   f x y = f y x
--   f x z <= f x y + f y z
newtype Distance =
  Distance {
    unDistance :: Double
  } deriving (Eq, Show, Ord, Num, AEq)

distance0 :: Distance
distance0 = Distance 0.0

euclidean :: FeatureVector -> FeatureVector -> Distance
euclidean (FeatureVector a) (FeatureVector b) =
  let
    diffs = VU.zipWith (\x y -> square $ x - y) a b
    diffs' = VU.modify Insertion.sort diffs
  in
  Distance . sqrt $ VU.foldl' (+) 0.0 diffs'

square :: Double -> Double
square x = x ^ (2 :: Int)
    

