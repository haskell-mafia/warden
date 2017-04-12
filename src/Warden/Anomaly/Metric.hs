{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Anomaly.Metric (
    Distance(..)
  ) where

import           P

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

