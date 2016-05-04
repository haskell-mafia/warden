{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Row.Internal where

import           Data.Bits ((.|.))
import qualified Data.ByteString as BS
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

prop_asciiToLower_arbitrary :: Property
prop_asciiToLower_arbitrary = forAll (choose (0, 1000)) $ \n -> forAll (vectorOf n (choose (0, 255))) $ \bytes ->
  let bs = BS.pack bytes
      r = asciiToLower bs
      bs1 = BS.map ((.|.) 0x20) r
      bs2 = BS.map ((.|.) 0x20) bs in
  (BS.all notUpper r, bs1) === (True, bs2)
  where
    notUpper w = not . BS.elem w $ BS.pack [0x41..0x5a] -- 'A'..'Z'

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
