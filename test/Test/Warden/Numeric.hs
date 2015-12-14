{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Warden.Numeric where

import           Data.AEq (AEq)

import           Disorder.Core.Property ((~~~))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary ()

import           Warden.Data
import           Warden.Numeric

prop_updateMinimum_positive :: Minimum -> Property
prop_updateMinimum_positive mn@(Minimum Nothing) = forAll (arbitrary :: Gen Double) $ \x ->
  (updateMinimum mn x) === (Minimum (Just x))
prop_updateMinimum_positive mn@(Minimum (Just c)) = forAll ((arbitrary :: Gen Double) `suchThat` (< c)) $ \x ->
  (updateMinimum mn x) === (Minimum (Just x))

prop_updateMaximum_positive :: Maximum -> Property
prop_updateMaximum_positive mn@(Maximum Nothing) = forAll (arbitrary :: Gen Double) $ \x ->
  (updateMaximum mn x) === (Maximum (Just x))
prop_updateMaximum_positive mx@(Maximum (Just c)) = forAll ((arbitrary :: Gen Double) `suchThat` (> c)) $ \x ->
  (updateMaximum mx x) === (Maximum (Just x))

prop_updateMinimum_negative :: Property
prop_updateMinimum_negative =
  forAll (arbitrary :: Gen Double) $ \c ->
    forAll ((arbitrary :: Gen Double) `suchThat` (>= c)) $ \x ->
      let mn = Minimum (Just c)
      in (updateMinimum mn x) === mn

prop_updateMinimum_associative :: Int -> Property
prop_updateMinimum_associative n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  associativity updateMinimum (Minimum Nothing) xs getMinimum

prop_updateMaximum_negative :: Property
prop_updateMaximum_negative =
  forAll (arbitrary :: Gen Double) $ \c ->
    forAll ((arbitrary :: Gen Double) `suchThat` (<= c)) $ \x ->
      let mx = Maximum (Just c)
      in (updateMaximum mx x) === mx

prop_updateMaximum_associative :: Int -> Property
prop_updateMaximum_associative n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  associativity updateMaximum (Maximum Nothing) xs getMaximum

prop_updateMean :: Int -> Property
prop_updateMean n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  let soq = finalizeMean $ foldl updateMean MeanInitial xs
      qos = if n > 0
              then Just . Mean $ ((sum xs) / (fromIntegral n))
              else Nothing
  in soq ~~~ qos

prop_updateMean_associative :: Int -> Property
prop_updateMean_associative n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  associativity updateMean MeanInitial xs finalizeMean

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
