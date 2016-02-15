{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Serial.Json.Marker where

import           Data.Aeson.Types (parseEither)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Test.Warden.Arbitrary ()
import           Test.QuickCheck

import           Warden.Data
import           Warden.Serial.Json.Marker

prop_FileMarker :: FileMarker -> Property
prop_FileMarker = tripping fromFileMarker (parseEither toFileMarker)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
