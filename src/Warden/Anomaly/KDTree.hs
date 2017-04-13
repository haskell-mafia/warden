{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Anomaly.KDTree (
    KDTree(..)
  , KD(..)
  , fromList
  , toList
  ) where

import qualified Data.List as L
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Unboxed as VU

import           P hiding (toList)

import           Warden.Anomaly.Data

-- | k-dimensional search tree for nearest neighbours in Cartesian metric
-- spaces.
data KDTree =
  KDTree {
    treeK :: !Dimensionality
  , treeRoot :: !(Maybe KD)
  } deriving (Eq, Show)

data KD =
  KD {
    kdLeft :: (Maybe KD)
  , kdPoint :: FeatureVector
  , kdRight :: (Maybe KD)
  } deriving (Eq, Show)

newtype Depth =
  Depth Int
  deriving (Eq, Show)

toList :: KDTree -> [FeatureVector]
toList (KDTree _ r) = toList' r

toList' :: Maybe KD -> [FeatureVector]
toList' Nothing = []
toList' (Just (KD l v r)) = v : ((toList' l) <> (toList' r))

fromList :: Dimensionality -> [FeatureVector] -> KDTree
fromList dim vs =
  KDTree dim (fromList' (Depth 0) dim vs)

-- | Build each layer of the KD tree by constructing a splitting hyperplane,
-- iterating through dimensions for the axis of the split.
--
-- FIXME: probably faster to precompute sorted slices
fromList' :: Depth -> Dimensionality -> [FeatureVector] -> Maybe KD
fromList' depth d vs =
  let
    pix = layerPivot depth d vs
    v = vs L.!! pix
    lvs = L.take pix vs
    rvs = L.drop (pix + 1) vs
  in
  case L.null vs of
    True ->
      Nothing
    False ->
      pure $ KD (fromList' (descend depth) d lvs) v (fromList' (descend depth) d rvs)

component :: Int -> FeatureVector -> Double
component k (FeatureVector v) =
  v VU.! k

descend :: Depth -> Depth
descend (Depth d) =
  Depth $ d + 1

layerPivot :: Depth -> Dimensionality -> [FeatureVector] -> Int
layerPivot (Depth i) (Dimensionality k) vs =
  let
    axis = i `mod` k
    n = length vs
    candidates = VU.zip (VU.generate n id) .
                   VU.fromList $ fmap (component axis) vs
    psed = VU.modify
             (\z -> Intro.partialSortBy (\x y -> compare (snd x) (snd y)) z (n `div` 2))
             candidates
  in
  fst $ psed VU.! (n `div` 2)
  
    
    
