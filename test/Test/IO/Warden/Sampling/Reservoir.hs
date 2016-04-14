{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Sampling.Reservoir where

import qualified Data.Vector.Unboxed as VU

import           Disorder.Core.IO

import           P

import           System.IO (IO)
import           System.Random.MWC (withSystemRandom)

import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()
import           Test.Warden.Arbitrary ()

import           Warden.Data
import           Warden.Sampling.Reservoir

prop_updateReservoirAcc :: ReservoirSize -> Property
prop_updateReservoirAcc rs = forAll (fmap VU.fromList $ vectorOf 1000 arbitrary) $ \xs -> testIO $ withSystemRandom $ \g -> do
  r <- newReservoirAcc rs
  ns <- fmap snd $ VU.foldM (update' g r) (RowCount 0, initialSampleCount) xs
  r' <- VU.freeze $ unReservoirAcc r
  let match = VU.all (\x -> isJust $ VU.find (== x) xs) r'
  pure $ (match, unSampleCount ns) === (True, unReservoirSize rs)
  where
    update' g r (ix, ns) x = do
      ns' <- updateReservoirAcc g ix r ns x
      pure $ (ix + (RowCount 1), ns')

prop_combineReservoirAcc_small :: ReservoirSize -> Property
prop_combineReservoirAcc_small (ReservoirSize rs) =
 forAll (choose (0, rs `div` 2)) $ \n ->
 forAll (choose (0, rs `div` 2)) $ \m ->
 forAll (fmap VU.fromList $ vectorOf n arbitrary) $ \xs ->
 forAll (fmap VU.fromList $ vectorOf m arbitrary) $ \ys ->
 testIO $ withSystemRandom $ \g -> do
   r1 <- fmap ReservoirAcc $ VU.thaw xs
   r2 <- fmap ReservoirAcc $ VU.thaw ys
   let sc1 = SampleCount $ VU.length xs
   let sc2 = SampleCount $ VU.length ys
   (SampleCount sc, ReservoirAcc r) <- combineReservoirAccs g (ReservoirSize rs) (sc1, r1) (sc2, r2)
   r' <- VU.freeze r
   pure $ (sc, VU.length r') === (n+m, n+m)

prop_combineReservoirAcc_large :: ReservoirSize -> Property
prop_combineReservoirAcc_large (ReservoirSize rs) =
 forAll (choose (rs `div` 2, rs * 2)) $ \n ->
 forAll (choose (rs `div` 2, rs * 2)) $ \m ->
 forAll (fmap VU.fromList $ vectorOf n arbitrary) $ \xs ->
 forAll (fmap VU.fromList $ vectorOf m arbitrary) $ \ys ->
 testIO $ withSystemRandom $ \g -> do
   r1 <- fmap ReservoirAcc $ VU.thaw xs
   r2 <- fmap ReservoirAcc $ VU.thaw ys
   let sc1 = SampleCount $ VU.length xs
   let sc2 = SampleCount $ VU.length ys
   (SampleCount sc, ReservoirAcc r) <- combineReservoirAccs g (ReservoirSize rs) (sc1, r1) (sc2, r2)
   r' <- VU.freeze r
   pure $ (sc, VU.length r') === (rs, rs)

prop_concatMutable :: VU.Vector Double -> VU.Vector Double -> Property
prop_concatMutable xs ys =
  let zs = xs VU.++ ys in testIO $ do
  xs' <- VU.thaw xs
  ys' <- VU.thaw ys
  zs' <- VU.freeze =<< concatMutable xs' ys'
  pure $ zs === zs'

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 })
