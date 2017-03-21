{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Anomaly.Data (
    FeatureVector(..)
  , fromDataRow
  , toMatrix
  ) where

import           Data.ByteString (ByteString)
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
