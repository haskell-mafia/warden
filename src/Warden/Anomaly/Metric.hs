{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Anomaly.Metric (
    Distance(..)
  , euclidean
  ) where

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
  } deriving (Eq, Show)

euclidean :: FeatureVector -> FeatureVector -> Distance
euclidean (FeatureVector a) (FeatureVector b) =
  Distance . sqrt . VU.foldl' (+) 0.0 $ VU.zipWith (\x y -> square $ x - y) a b

square :: Double -> Double
square x = x ^ (2 :: Int)
    

