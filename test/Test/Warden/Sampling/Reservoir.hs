{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Warden.Sampling.Reservoir where


import           P

import           System.IO

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Sampling.Reservoir

prop_xCDF_probability :: XDist -> Double -> Property
prop_xCDF_probability d x =
  let (Probability p) = xCDF d x in
  (p > 1, p < 0) === (False, False)

prop_xQuantile_positive :: XDist -> Probability -> Property
prop_xQuantile_positive d p =
  let x = xQuantile d p in
  (x > 0.0) === True

return []
tests :: IO Bool
tests = $quickCheckAll

