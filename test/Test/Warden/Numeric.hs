{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Test.Warden.Numeric where

import           Data.AEq (AEq)
import           Data.List (take)

import           Disorder.Core.Property ((~~~))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Numeric

unstableMean :: (Num a, Fractional a) => [a] -> a
unstableMean xs =
  (sum xs) / (fromIntegral $ length xs)

unstableVariance :: (Num a, Fractional a) => a -> [a] -> a
unstableVariance mu xs =
  (foldr (\v acc -> acc + ((v - mu) ^ two)) 0.0 xs) / fromIntegral (length xs)
  where
    two :: Int
    two = 2

associativity :: (Show c, AEq c)
              => (a -> b -> a) -> a -> [b] -> (a -> c) -> Property
associativity f y0 xs g =
  left' ~~~ right'
  where
    left' = g $ foldl f y0 xs

    right' = g $ foldr f' y0 xs

    f' a b = f b a

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
  let mda = foldl updateMeanDev MeanDevInitial xs
      nsMeanDev = finalizeMeanDev mda
      mu = unstableMean xs
      var = unstableVariance mu xs
      sd = StdDev $ sqrt var
      uMeanDev = (Mean mu, sd) in
  (nsMeanDev, Just (n+1)) ~~~ (uMeanDev, meanDevCount mda)
  where
    meanDevCount MeanDevInitial = Nothing
    meanDevCount (MeanDevAcc _ _ (Count c)) = Just c

prop_updateMeanDev_associative :: Int -> Property
prop_updateMeanDev_associative n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  associativity updateMeanDev MeanDevInitial xs finalizeMeanDev

prop_combineMeanDevAcc :: Property
prop_combineMeanDevAcc = forAll smallPositiveEven $ \n -> forAll (vectorOf n arbitrary) $ \xs ->
  let m = n `div` 2
      mu = unstableMean xs
      var = unstableVariance mu xs
      mda = MeanDevAcc (MeanAcc mu) (Just $ Variance var) (Count n)
      xs1 = take m xs
      xs2 = drop m xs
      mu1 = MeanAcc $ unstableMean xs1
      mu2 = MeanAcc $ unstableMean xs2
      var1 = Just . Variance $ unstableVariance (unMeanAcc mu1) xs1
      var2 = Just . Variance $ unstableVariance (unMeanAcc mu2) xs2
      mda1 = MeanDevAcc mu1 var1 (Count m)
      mda2 = MeanDevAcc mu2 var2 (Count m)
      mda' = combineMeanDevAcc mda1 mda2 in
  mda ~~~ mda'

return []
tests :: IO Bool
tests = $quickCheckAll
