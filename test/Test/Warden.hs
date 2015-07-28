{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Warden where

import           P

import           Control.Lens              hiding (each)
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Pipes
import           System.IO
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary

import           Warden.Data

prop_tok_count_state :: FieldCount -> RowCount -> Property
prop_tok_count_state i n = forAll (vectorOf (getRowCount n) $ tokenizedRow i) $ \rows ->
  let st = runIdentity . countFields $ each rows in
         ((st ^. totalRows) === fromIntegral (getRowCount n))
    .&&. ((st ^. badRows) === 0)
    .&&. ((st ^. numFields) === [(getFieldCount i)])
    .&&. ((V.length <$> (st ^. fieldCounts)) === Just (getFieldCount i))
    .&&. ((hasBroken (st ^. fieldCounts)) === V.fromList [])
 where
  hasBroken (Just v) = V.filter (isJust . M.lookup LooksBroken . fst) v
  hasBroken Nothing  = V.fromList []

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
