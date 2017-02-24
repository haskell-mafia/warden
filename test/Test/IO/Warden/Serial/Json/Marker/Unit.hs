{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Serial.Json.Marker.Unit where

import           Control.Monad.Trans.Resource (runResourceT)

import           Disorder.Core.IO (testIO)

import           P

import           System.IO (IO)

import           Test.QuickCheck

import           Warden.Marker

import           X.Control.Monad.Trans.Either (runEitherT)

prop_FileMarker_compatibility_v1 :: Property
prop_FileMarker_compatibility_v1 = testIO $ do
  fm <- runResourceT . runEitherT $ readFileMarker' "test/data/serial/json/marker/file_marker_v1.json"
  pure $ isRight fm === True

prop_ViewMarker_compatibility_v1 :: Property
prop_ViewMarker_compatibility_v1 = testIO $ do
  vm <- runResourceT . runEitherT $ readViewMarker "test/data/serial/json/marker/view_marker_v1.json"
  pure $ isRight vm === True

prop_FileMarker_compatibility_v2 :: Property
prop_FileMarker_compatibility_v2 = testIO $ do
  fm <- runResourceT . runEitherT $ readFileMarker' "test/data/serial/json/marker/file_marker_v2.json"
  pure $ isRight fm === True

prop_ViewMarker_compatibility_v2 :: Property
prop_ViewMarker_compatibility_v2 = testIO $ do
  vm <- runResourceT . runEitherT $ readViewMarker "test/data/serial/json/marker/view_marker_v2.json"
  pure $ isRight vm === True

prop_FileMarker_compatibility_v3 :: Property
prop_FileMarker_compatibility_v3 = testIO $ do
  fm <- runResourceT . runEitherT $ readFileMarker' "test/data/serial/json/marker/file_marker_v3.json"
  pure $ isRight fm === True

prop_ViewMarker_compatibility_v3 :: Property
prop_ViewMarker_compatibility_v3 = testIO $ do
  vm <- runResourceT . runEitherT $ readViewMarker "test/data/serial/json/marker/view_marker_v3.json"
  pure $ isRight vm === True

prop_FileMarker_compatibility_v4 :: Property
prop_FileMarker_compatibility_v4 = testIO $ do
  fm <- runResourceT . runEitherT $ readFileMarker' "test/data/serial/json/marker/file_marker_v4.json"
  pure $ isRight fm === True

prop_ViewMarker_compatibility_v4 :: Property
prop_ViewMarker_compatibility_v4 = testIO $ do
  vm <- runResourceT . runEitherT $ readViewMarker "test/data/serial/json/marker/view_marker_v4.json"
  pure $ isRight vm === True


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
