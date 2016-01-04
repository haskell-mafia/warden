{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Warden.Fold where

import           Control.Lens              hiding (each)

import qualified Data.Map                  as M
import qualified Data.Vector               as V

import           P

import           Pipes

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Fold

prop_tok_count_state :: FieldCount -> RowCount -> Property
prop_tok_count_state i n = forAll (vectorOf (unRowCount n) $ tokenizedRow i) $ \rows ->
  let st = runIdentity . countFields $ each rows in
         ((st ^. totalRows) === fromIntegral (unRowCount n))
    .&&. ((st ^. badRows) === 0)
    .&&. ((st ^. numFields) === [i])
    .&&. ((V.length <$> (st ^. fieldCounts)) === Just (unFieldCount i))
    .&&. ((hasBroken (st ^. fieldCounts)) === V.fromList [])
 where
  hasBroken (Just v) = V.filter (isJust . M.lookup LooksBroken . fst) v
  hasBroken Nothing  = V.fromList []

return []
tests :: IO Bool
tests = $quickCheckAll
