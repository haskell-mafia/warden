{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Test.Warden.Numeric where

import           Data.AEq (AEq)

import           Disorder.Core.Property ((~~~))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Numeric

prop_updateMinimum_positive :: Minimum -> Property
prop_updateMinimum_positive NoMinimum = forAll (arbitrary :: Gen Double) $ \x ->
  (updateMinimum NoMinimum x) === (Minimum x)
prop_updateMinimum_positive mn@(Minimum c) = forAll ((arbitrary :: Gen Double) `suchThat` (< c)) $ \x ->
  (updateMinimum mn x) === (Minimum x)

prop_updateMaximum_positive :: Maximum -> Property
prop_updateMaximum_positive NoMaximum = forAll (arbitrary :: Gen Double) $ \x ->
  (updateMaximum NoMaximum x) === (Maximum x)
prop_updateMaximum_positive mx@(Maximum c) = forAll ((arbitrary :: Gen Double) `suchThat` (> c)) $ \x ->
  (updateMaximum mx x) === (Maximum x)

prop_updateMinimum_negative :: Property
prop_updateMinimum_negative =
  forAll (arbitrary :: Gen Double) $ \c ->
    forAll ((arbitrary :: Gen Double) `suchThat` (>= c)) $ \x ->
      let mn = Minimum c
      in (updateMinimum mn x) === mn

prop_updateMinimum_associative :: Int -> Property
prop_updateMinimum_associative n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  associativity updateMinimum NoMinimum xs $ \case
    NoMinimum -> 0.0
    Minimum v -> v

prop_updateMaximum_negative :: Property
prop_updateMaximum_negative =
  forAll (arbitrary :: Gen Double) $ \c ->
    forAll ((arbitrary :: Gen Double) `suchThat` (<= c)) $ \x ->
      let mx = Maximum c
      in (updateMaximum mx x) === mx

prop_updateMaximum_associative :: Int -> Property
prop_updateMaximum_associative n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  associativity updateMaximum NoMaximum xs $ \case
    NoMaximum -> 0.0
    Maximum v -> v

prop_updateMeanDev :: NPlus -> Property
prop_updateMeanDev (NPlus n) = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  let nsMeanDev = finalizeMeanDev $ foldl updateMeanDev MeanDevInitial xs

      mu = (sum xs) / (fromIntegral n)
      var = foldr (\v acc -> acc + ((v - mu) ^ two)) 0.0 xs
      sd = StdDev $ sqrt var
      uMeanDev = Just (Mean mu, sd)
  in nsMeanDev ~~~ uMeanDev
  where
    two :: Integer
    two = 2

prop_updateMeanDev_associative :: Int -> Property
prop_updateMeanDev_associative n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  associativity updateMeanDev MeanDevInitial xs finalizeMeanDev

associativity :: (Show c, AEq c)
              => (a -> b -> a) -> a -> [b] -> (a -> c) -> Property
associativity f y0 xs g =
  left' ~~~ right'
  where
    left' = g $ foldl f y0 xs

    right' = g $ foldr f' y0 xs

    f' a b = f b a

return []
tests :: IO Bool
tests = $quickCheckAll
