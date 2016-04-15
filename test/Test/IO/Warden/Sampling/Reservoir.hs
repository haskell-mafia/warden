{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Sampling.Reservoir where

import qualified Data.Vector.Unboxed as VU

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property (failWith)

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
  ra <- fmap snd $ VU.foldM (update' g) (RowCount 0, NoReservoirAcc) xs
  case ra of
    NoReservoirAcc ->
      pure $ failWith "Reservoir unexpectedly uninitialized."
    ReservoirAcc r ns -> do
      r' <- VU.freeze $ unReservoir r
      let match = VU.all (\x -> isJust $ VU.find (== x) xs) r'
      pure $ (match, unSampleCount ns) === (True, unReservoirSize rs)
  where
    update' g (ix, ra) x = do
      ra' <- updateReservoirAcc g rs ix ra x
      pure $ (ix + (RowCount 1), ra')

prop_combineReservoirAcc_small :: ReservoirSize -> Property
prop_combineReservoirAcc_small (ReservoirSize rs) =
 forAll (choose (0, rs `div` 2)) $ \n ->
 forAll (choose (0, rs `div` 2)) $ \m ->
 forAll (fmap VU.fromList $ vectorOf n arbitrary) $ \xs ->
 forAll (fmap VU.fromList $ vectorOf m arbitrary) $ \ys ->
 testIO $ withSystemRandom $ \g -> do
   let sc1 = SampleCount $ VU.length xs
   let sc2 = SampleCount $ VU.length ys
   ra1 <- fmap (flip ReservoirAcc sc1 . Reservoir) $ VU.thaw xs
   ra2 <- fmap (flip ReservoirAcc sc2 . Reservoir) $ VU.thaw ys
   ra <- combineReservoirAcc g (ReservoirSize rs) ra1 ra2
   case ra of
     NoReservoirAcc ->
       pure $ failWith "Unexpectedly uninitialized reservoir."
     ReservoirAcc (Reservoir r) (SampleCount sc) -> do
       r' <- VU.freeze r
       pure $ (sc, VU.length r') === (n+m, n+m)

prop_combineReservoirAcc_large :: ReservoirSize -> Property
prop_combineReservoirAcc_large (ReservoirSize rs) =
 forAll (choose (rs `div` 2 + 1, rs * 2)) $ \n ->
 forAll (choose (rs `div` 2 + 1, rs * 2)) $ \m ->
 forAll (fmap VU.fromList $ vectorOf n arbitrary) $ \xs ->
 forAll (fmap VU.fromList $ vectorOf m arbitrary) $ \ys ->
 testIO $ withSystemRandom $ \g -> do
   let sc1 = SampleCount $ VU.length xs
   let sc2 = SampleCount $ VU.length ys
   ra1 <- fmap (flip ReservoirAcc sc1 . Reservoir) $ VU.thaw xs
   ra2 <- fmap (flip ReservoirAcc sc2 . Reservoir) $ VU.thaw ys
   ra <- combineReservoirAcc g (ReservoirSize rs) ra1 ra2
   case ra of
     NoReservoirAcc ->
       pure $ failWith "Unexpectedly uninitialized reservoir."
     ReservoirAcc (Reservoir r) (SampleCount sc) -> do
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
