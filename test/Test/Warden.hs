{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden where

import P

import Control.Lens hiding (each)
import qualified Data.Vector as V
import Pipes
import System.IO
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Test.Warden.Arbitrary

import Warden.Data

prop_valid_parse_state :: FieldCount -> RowCount -> Property
prop_valid_parse_state i n = forAll (vectorOf (getRowCount n) $ validRow i) $ \rows ->
  let st = runIdentity . countFields $ each rows in
       ((st ^. totalRecords) === fromIntegral (getRowCount n))
  .&&. ((st ^. badRecords) === 0)
  .&&. ((st ^. numFields) === [(getFieldCount i)])
  .&&. (V.length (st ^. fieldCounts) == (getFieldCount i))

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
