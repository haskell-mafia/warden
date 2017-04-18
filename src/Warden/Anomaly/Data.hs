{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Anomaly.Data (
    FeatureVector(..)
  , Features(..)
  , featuresFromList
  , Dimensionality(..)
  , dimensionality
  , fromDataRow
  , toMatrix
  ) where

import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           Numeric.LinearAlgebra (R)
import qualified Numeric.LinearAlgebra as HM

import           P

import           Warden.Data.Numeric
import           Warden.Data.Row
import           Warden.Row (parseProbablyNumeric)

newtype FeatureVector =
  FeatureVector {
    unFeatureVector :: VU.Vector Double
  } deriving (Eq, Show)

numFeatures :: FeatureVector -> Dimensionality
numFeatures = Dimensionality . VU.length . unFeatureVector

-- | Contains one or more feature vectors. All feature vectors have the same
-- dimensionality.
newtype Features =
  Features {
    unFeatures :: V.Vector FeatureVector
  } deriving (Eq, Show)

newtype Dimensionality =
  Dimensionality {
    unDimensionality :: Int
  } deriving (Eq, Show)

dimensionality :: Features -> Dimensionality
dimensionality (Features fvs) =
  numFeatures $ V.head fvs

featuresFromList :: NonEmpty FeatureVector -> Maybe' Features
featuresFromList (v:|vs) =
  let
    dim = numFeatures v
    bads = filter (\x -> numFeatures x /= dim) vs
  in
  case bads of
    [] ->
      Just' . Features . V.fromList . NE.toList $ v :| vs
    _ ->
      Nothing'

getNumericField :: V.Vector ByteString -> Int -> Maybe' Double
getNumericField fs ix = do
  f <- strictMaybe $ fs V.!? ix
  case parseProbablyNumeric f of
    NoNumericField ->
      Nothing'
    MNumericField (NumericField x) ->
      pure x

fromDataRow :: [Int] -> Row -> Maybe' FeatureVector
fromDataRow nixes (SVFields fs) = do
  nums <- mapM (getNumericField fs) nixes
  pure . FeatureVector $ VU.fromList nums
fromDataRow _ (RowFailure _) =
  Nothing'

toMatrix :: [FeatureVector] -> HM.Matrix R
toMatrix =
  HM.fromRows . fmap (HM.vector . VU.toList . unFeatureVector)
