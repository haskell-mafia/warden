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
--
-- The k-d tree is a binary space-partitioning tree. Each node is a
-- point in a k-dimensional Euclidean vector space which has a
-- co-ordinate system. Each non-leaf node has two children, and
-- divides the rest of the tree into two half-spaces along an implicit
-- "pivot plane".
--
-- The pivot plane is defined by assigning each node \( v \) a splitting
-- dimension; a node's splitting dimension \( s \) is determined by its depth
-- in the tree. The hyperplane perpendicular to \( s \) is the pivot plane
-- at \( v \), splitting the subtree rooted at \( v \) such that every
-- node which is smaller in the \( s \) dimension will appear to the
-- left of \( v \) and vice-versa for nodes larger in \( s \).
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

-- | Find the index of the node we'll use to construct the pivot
-- plane. This is the median of the input nodes in the splitting
-- dimension, which is determined by the node's depth in the tree.
layerPivot :: Depth -> Dimensionality -> [FeatureVector] -> Int
layerPivot (Depth i) (Dimensionality k) vs =
  let
    -- Take the splitting dimension from our depth in the tree.
    splittingDim = i `mod` k
    n = length vs
    -- Build a vector from our input points decorated by their
    -- indices; we want to sort on value but return an index.
    candidates = VU.zip (VU.generate n id) .
                   VU.fromList $ fmap (component splittingDim) vs
    -- Sort up to half the length of the input list so we can find
    -- an approximate median.
    psed = VU.modify
             (\z -> Intro.partialSortBy (\x y -> compare (snd x) (snd y)) z (n `div` 2))
             candidates
  in
  fst $ psed VU.! (n `div` 2)
