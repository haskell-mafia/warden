{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Random where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as Intro

import           Disorder.Core.IO

import           P

import           System.IO (IO)
import           System.Random.MWC (withSystemRandom)

import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()
import           Test.Warden.Arbitrary ()

import           Warden.Random

-- This will fail with probability 2/10000! if everything is working correctly.
prop_uniformShuffle :: Property
prop_uniformShuffle = testIO $ withSystemRandom $ \g ->
  let xs = VU.fromList ([1..10000] :: [Double]) in do
  v <- VU.thaw xs
  uniformShuffle g v
  ys <- VU.freeze v
  uniformShuffle g v
  zs <- VU.freeze v
  pure $ (xs /= ys, ys /= zs, VU.modify Intro.sort xs, VU.modify Intro.sort xs) === (True, True, VU.modify Intro.sort ys, VU.modify Intro.sort zs)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
