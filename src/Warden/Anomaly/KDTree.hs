{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Anomaly.KDTree (
    KDTree(..)
  ) where

import           P

import           Warden.Anomaly.Data

data KDTree =
  KDTree {
    treeK :: !Dimensionality
  , treeRoot :: !KD
  } deriving (Eq, Show)

data KD =
  KD {
    kdLeft :: (Maybe KD)
  , kdPoint :: FeatureVector
  , kdRight :: (Maybe KD)
  } deriving (Eq, Show)
