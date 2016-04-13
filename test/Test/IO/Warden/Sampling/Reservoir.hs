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

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
