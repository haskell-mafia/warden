{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Marker where

import           Disorder.Core (tripping)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data

prop_tripping_filemarker :: ValidViewFile -> Property
prop_tripping_filemarker (ValidViewFile v vf) =
  tripping fileToMarker (markerToFile v) vf

prop_tripping_viewmarker :: View -> Property
prop_tripping_viewmarker =
  tripping viewToMarker markerToView

return []
tests :: IO Bool
tests = $quickCheckAll

