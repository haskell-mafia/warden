{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Warden where

import           Control.Lens              hiding (each)
import qualified Data.Aeson                as A
import           Data.Attoparsec.Text
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Disorder.Core.Tripping
import           P
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

prop_roundtrip_parsed_field :: ParsedField -> Property
prop_roundtrip_parsed_field = tripping renderParsedField (parseOnly field)

prop_roundtrip_json_numericsummary :: NumericSummary -> Property
prop_roundtrip_json_numericsummary = tripping A.encode A.decode

return []
tests :: IO Bool
tests = $quickCheckAll
