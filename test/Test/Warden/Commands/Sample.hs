{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Commands.Sample where

import           Data.List (take, repeat, nub)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Warden.Commands.Sample

prop_identical :: [Int] -> Property
prop_identical xs =
  let
    ident = elem (length $ nub xs) [0, 1]
    ident' = identical xs
  in
  ident === ident'

prop_identical_id :: Int -> Property
prop_identical_id x =
  forAll (choose (0, 20)) $ \n ->
    let
      xs = take n $ repeat x
    in
    identical xs === True

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
