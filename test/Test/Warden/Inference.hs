{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Inference where

import           Data.List (take, repeat)
import qualified Data.List.NonEmpty as NE

import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data
import           Warden.Inference

prop_viewMarkerMismatch_same :: ViewMarker -> Property
prop_viewMarkerMismatch_same vm =
  (isRight $ viewMarkerMismatch vm vm) === True

prop_viewMarkerMismatch_different :: ViewMarker -> UniquePair View -> Property
prop_viewMarkerMismatch_different vm (UniquePair va vb) =
  let vma = vm { vmView = va }
      vmb = vm { vmView = vb } in
  (isRight $ viewMarkerMismatch vma vmb) === False

prop_validateViewMarkers_same :: ViewMarker -> Property
prop_validateViewMarkers_same vm = forAll (choose (1, 100)) $ \n ->
  let vms = NE.fromList . take n $ repeat vm in
  (validateViewMarkers vms) === (Right ())


return []
tests :: IO Bool
tests = $quickCheckAll
