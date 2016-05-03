{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Row.Internal where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets)

import           P

import           System.IO (IO)

import           Test.QuickCheck

import           Warden.Row.Internal

prop_asciiToLower :: Property
prop_asciiToLower = forAll (elements muppets) $ \t ->
  let bs = T.encodeUtf8 $ T.toUpper t
      r = T.decodeUtf8 $ asciiToLower bs in
  r === t

prop_asciiToLower_numbers :: Double -> Property
prop_asciiToLower_numbers n =
  let bs = T.encodeUtf8 $ renderFractional n
      r = asciiToLower bs in
  r === bs

return []
tests :: IO Bool
tests = $quickCheckAll
