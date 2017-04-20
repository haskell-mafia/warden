{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Commands.Sample where

import           Control.Monad.IO.Class (liftIO)

import           Disorder.Core.IO (testIO)

import           P

import           System.Directory (removeFile)
import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.IO.Warden
import           Test.Warden.Arbitrary ()

import           Warden.Data
import           Warden.Marker

import           X.Control.Monad.Trans.Either (bracketEitherT')

prop_writeViewMarker :: NonEmpty ViewMarker -> Property
prop_writeViewMarker vms =
  forAll genNumericFieldSummary $ \nfs ->
  testIO $ withTestView $ \v -> unsafeWarden $ do
  let
    vms' = fmap (\vm -> vm { vmView = v }) vms
    vms'' = fmap (setNumericFieldSummary nfs) vms'
    fps = fmap viewMarkerPath vms''

  mapM_ writeViewMarker vms''
  vm'' <- readViewMarker $ viewMarkerPath vm'
  pure $ vm'' === vm'

setNumericFieldSummary :: NumericFieldSummary -> ViewMarker -> ViewMarker
setNumericFieldSummary nfs vm =
  let
    rcs = vmViewCounts $ vmMetadata vm
    rcs' = rcs { rcsNumericSummary = nfs }
    meta' = vmMetadata vm { vmViewCounts = rcs' }
  in
  vm { vmMetadata = meta' }

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
