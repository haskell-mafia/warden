{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Schema where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data.Schema

prop_tripping_FileFormat :: FileFormat -> Property
prop_tripping_FileFormat = tripping renderFileFormat parseFileFormat

return []
tests :: IO Bool
tests = $quickCheckAll
