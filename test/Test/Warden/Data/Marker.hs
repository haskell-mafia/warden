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

prop_tripping_viewmarker :: View -> Property
prop_tripping_viewmarker =
  tripping viewToMarker markerToView

prop_combineFileMarker :: FileMarker -> FileMarker -> Property
prop_combineFileMarker fm1 fm2 =
  let fm2' = fm2 {
      fmVersion = fmVersion fm1
    , fmViewFile = fmViewFile fm1
    } in
  (isRight $ combineFileMarker fm1 fm2') === True

prop_combineFileMarker_file :: Property
prop_combineFileMarker_file = forAll (arbitrary `suchThat` (\(a, b) -> fmViewFile a /= fmViewFile b)) $ \(fm1, fm2) ->
  let fm2' = fm2 {
      fmVersion = fmVersion fm1
    } in
  (isLeft $ combineFileMarker fm1 fm2') === True

return []
tests :: IO Bool
tests = $quickCheckAll
