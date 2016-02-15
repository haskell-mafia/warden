{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Marker where

import           Disorder.Core.IO

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.IO.Warden
import           Test.Warden.Arbitrary ()

import           Warden.Data
import           Warden.Marker

prop_writeFileMarker :: FileMarker -> Property
prop_writeFileMarker fm = testIO $ withTestFile $ \vf _ -> unsafeWarden $ do
  let fm' = fm { fmViewFile = vf }
  writeFileMarker fm'
  let fp = fileToMarker $ fmViewFile fm'
  fm'' <- readFileMarker fp
  pure $ fm'' === fm'

prop_writeViewMarker :: ViewMarker -> Property
prop_writeViewMarker vm = testIO $ withTestView $ \v -> unsafeWarden $ do
  let vm' = vm { vmView = v }
  writeViewMarker vm'
  let fp = viewToMarker $ vmView vm'
  vm'' <- readViewMarker fp
  pure $ vm'' === vm'

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
