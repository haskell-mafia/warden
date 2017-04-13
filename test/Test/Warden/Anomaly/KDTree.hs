{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Anomaly.KDTree where

import           Disorder.Core.Property ((=\\=))

import           P hiding (toList)

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary

import           Warden.Anomaly.Data
import           Warden.Anomaly.KDTree

prop_fromList :: Dimensionality -> Property
prop_fromList d =
  forAll (genFeatureVectors (unDimensionality d)) $ \vs ->
    let
      t = fromList d vs
    in
    toList t =\\= vs

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
