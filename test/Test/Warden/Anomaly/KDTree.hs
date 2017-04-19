{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Anomaly.KDTree where

import qualified Data.Vector as V
import           Disorder.Core.Property ((=\\=))

import           P hiding (toList)

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary

import           Warden.Anomaly.Data
import           Warden.Anomaly.KDTree

prop_fromFeatures :: Dimensionality -> Property
prop_fromFeatures d =
  forAll (genFeatures (unDimensionality d)) $ \fs ->
    let
      t = fromFeatures fs
    in
    (V.toList $ unFeatures $ toFeatures t) =\\= (V.toList $ unFeatures fs)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
