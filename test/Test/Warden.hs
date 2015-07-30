{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Warden where

import           Control.Lens              hiding (each)
import           Data.Attoparsec.Text
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Disorder.Aeson
import           Disorder.Core.Tripping
import           Disorder.Core.Property
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
prop_roundtrip_json_numericsummary = jsonProp

prop_updateminimum_positive :: Minimum -> Property
prop_updateminimum_positive mn@(Minimum Nothing) = forAll (arbitrary :: Gen Double) $ \x ->
  (updateMinimum mn x) === (Minimum (Just x))
prop_updateminimum_positive mn@(Minimum (Just c)) = forAll ((arbitrary :: Gen Double) `suchThat` (< c)) $ \x ->
  (updateMinimum mn x) === (Minimum (Just x))

prop_updatemaximum_positive :: Maximum -> Property
prop_updatemaximum_positive mn@(Maximum Nothing) = forAll (arbitrary :: Gen Double) $ \x ->
  (updateMaximum mn x) === (Maximum (Just x))
prop_updatemaximum_positive mx@(Maximum (Just c)) = forAll ((arbitrary :: Gen Double) `suchThat` (> c)) $ \x ->
  (updateMaximum mx x) === (Maximum (Just x))

prop_updateminimum_negative :: Property
prop_updateminimum_negative =
  forAll (arbitrary :: Gen Double) $ \c ->
    forAll ((arbitrary :: Gen Double) `suchThat` (>= c)) $ \x ->
      let mn = Minimum (Just c)
      in (updateMinimum mn x) === mn

prop_updatemaximum_negative :: Property
prop_updatemaximum_negative =
  forAll (arbitrary :: Gen Double) $ \c ->
    forAll ((arbitrary :: Gen Double) `suchThat` (<= c)) $ \x ->
      let mx = Maximum (Just c)
      in (updateMaximum mx x) === mx

prop_updatemean :: Int -> Property
prop_updatemean n = forAll (vectorOf n (arbitrary :: Gen Double)) $ \xs ->
  let soq = getMean $ foldl (updateMean n) (Mean Nothing) xs
      qos = if n > 0
              then Just ((sum xs) / (fromIntegral n))
              else Nothing
  in soq ~~~ qos

return []
tests :: IO Bool
tests = $quickCheckAll
