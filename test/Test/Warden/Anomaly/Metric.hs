{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Test.Warden.Anomaly.Metric where

import           Data.AEq ((~==), AEq)

import           Disorder.Core.Property ((=/=))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary

import           Warden.Anomaly.Data
import           Warden.Anomaly.Metric

metricLaws
  :: (FeatureVector -> FeatureVector -> Distance)
  -> Property
metricLaws f =
  forAll (choose (1, 20)) $ \n ->
  forAll ((,,) <$> genFeatureVector n <*> genFeatureVector n <*> genFeatureVector n) $ \(a, b, c) ->
    conjoin [
        separation f a b
      , indiscernibles f a b
      , symmetry f a b
      , triangle f a b c
      ]

separation
  :: (FeatureVector -> FeatureVector -> Distance)
  -> FeatureVector
  -> FeatureVector
  -> Property
separation f a b =
  counterexample "separation" $ (f a b >= distance0) === True

indiscernibles
  :: (FeatureVector -> FeatureVector -> Distance)
  -> FeatureVector
  -> FeatureVector
  -> Property
indiscernibles f a b =
  case a == b of
    True ->
      (f a b) === distance0
    False ->
      (f a b) =/= distance0

symmetry
  :: (FeatureVector -> FeatureVector -> Distance)
  -> FeatureVector
  -> FeatureVector
  -> Property
symmetry f a b =
  (f a b) === (f b a)

triangle
  :: (FeatureVector -> FeatureVector -> Distance)
  -> FeatureVector
  -> FeatureVector
  -> FeatureVector
  -> Property
triangle f a b c =
  counterexample "triangle inequality" $
    ((f a c) <~ ((f a b) + (f b c))) === True

(<~) :: (AEq a, Ord a) => a -> a -> Bool
x <~ y =
  or [
      x ~== y
    , x > y
    ]

prop_metric_euclidean :: Property
prop_metric_euclidean =
  metricLaws euclidean

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
