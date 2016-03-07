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

prop_combineFileMarker :: WardenParams -> FileMarker -> FileMarker -> Property
prop_combineFileMarker wps fm1 fm2 =
  let fm2' = fm2 {
      fmVersion = fmVersion fm1
    , fmViewFile = fmViewFile fm1
    } in
  (isRight $ combineFileMarker wps fm1 fm2') === True

prop_combineFileMarker_file :: WardenParams -> Property
prop_combineFileMarker_file wps = forAll (arbitrary `suchThat` (\(a, b) -> fmViewFile a /= fmViewFile b)) $ \(fm1, fm2) ->
  let fm2' = fm2 {
      fmVersion = fmVersion fm1
    } in
  (isLeft $ combineFileMarker wps fm1 fm2') === True

return []
tests :: IO Bool
tests = $quickCheckAll
