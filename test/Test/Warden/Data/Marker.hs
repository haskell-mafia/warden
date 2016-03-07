{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Marker where

import           Disorder.Core (tripping)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data

prop_tripping_filemarker :: ViewFile -> Property
prop_tripping_filemarker =
  tripping fileToMarker markerToFile

return []
tests :: IO Bool
tests = $quickCheckAll
