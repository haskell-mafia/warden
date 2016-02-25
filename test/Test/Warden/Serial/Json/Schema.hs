{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Serial.Json.Schema where

import           Data.Aeson.Types (parseEither)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Test.Warden.Arbitrary ()
import           Test.QuickCheck

import           Warden.Data
import           Warden.Serial.Json.Schema

prop_Schema :: Schema -> Property
prop_Schema = tripping fromSchema (parseEither toSchema)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
