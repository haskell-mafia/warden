{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.IO.Warden.Serial.Json.Marker.Unit where

import           Control.Monad.Trans.Resource (runResourceT)

import           Disorder.Core.IO (testIO)

import           P

import           System.IO (IO, FilePath)

import           Test.QuickCheck

import           Warden.Marker

import           X.Control.Monad.Trans.Either (runEitherT)

fileMarkerCompatibility :: FilePath -> Property
fileMarkerCompatibility fp = testIO $ do
  fm <- runResourceT . runEitherT $ readFileMarker' fp
  pure $ isRight fm === True

viewMarkerCompatibility :: FilePath -> Property
viewMarkerCompatibility fp = testIO $ do
  vm <- runResourceT . runEitherT $ readViewMarker fp
  pure $ isRight vm === True

prop_FileMarker_compatibility_v1 =
  fileMarkerCompatibility "test/data/serial/json/marker/file_marker_v1.json"

prop_ViewMarker_compatibility_v1 =
  viewMarkerCompatibility "test/data/serial/json/marker/view_marker_v1.json"

prop_FileMarker_compatibility_v2 =
  fileMarkerCompatibility "test/data/serial/json/marker/file_marker_v2.json"

prop_ViewMarker_compatibility_v2 =
  viewMarkerCompatibility "test/data/serial/json/marker/view_marker_v2.json"

prop_FileMarker_compatibility_v3 =
  fileMarkerCompatibility "test/data/serial/json/marker/file_marker_v3.json"

prop_ViewMarker_compatibility_v3 =
  viewMarkerCompatibility "test/data/serial/json/marker/view_marker_v3.json"

prop_FileMarker_compatibility_v4 =
  fileMarkerCompatibility "test/data/serial/json/marker/file_marker_v4.json"

prop_ViewMarker_compatibility_v4 =
  viewMarkerCompatibility "test/data/serial/json/marker/view_marker_v4.json"

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
