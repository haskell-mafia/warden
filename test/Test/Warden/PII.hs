{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.PII where

import qualified Data.Text.Encoding as T

import           Disorder.Core.Property (neg)
import           Disorder.Core.UniquePair (UniquePair(..))
import           Disorder.Corpus (southpark, muppets)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.PII

prop_checkPII_pos :: Property
prop_checkPII_pos = forAll genPII $ \(bs, t) ->
  let r = checkPII bs in
  r === Just t

prop_checkPII_neg :: Integer -> Property
prop_checkPII_neg n = forAll (elements southpark) $ \bs ->
  let r1 = checkPII bs
      r2 = checkPII . T.encodeUtf8 $ renderIntegral n in
  (r1, r2) === (Nothing, Nothing)

prop_updatePIIObservations_neg :: FieldIndex -> MaxPIIObservations -> PIIObservations -> Property
prop_updatePIIObservations_neg fi mpo o = forAll ((fmap T.encodeUtf8) <$> (vectorOf 1000 (elements muppets))) $ \fs ->
  let r = foldl (updatePIIObservations mpo fi) o fs in
  r === o

prop_updatePIIObservations_pos :: FieldIndex -> MaxPIIObservations -> PIIObservations -> Property
prop_updatePIIObservations_pos fi mpo o = forAll (vectorOf 1000 genPII) $ \fs ->
  let r = foldl (updatePIIObservations mpo fi) o (fst <$> fs) in
  o /= TooManyPIIObservations ==> neg $ r === o

prop_updatePIIObservations_pos_toomany :: FieldIndex -> MaxPIIObservations -> Property
prop_updatePIIObservations_pos_toomany fi mpo = forAll (vectorOf 1000 genPII) $ \fs ->
  let r = foldl (updatePIIObservations mpo fi) TooManyPIIObservations (fst <$> fs) in
  r === TooManyPIIObservations

prop_combinePIIObservations_commutative :: MaxPIIObservations -> UniquePair PIIObservations -> Property
prop_combinePIIObservations_commutative mpo = commutativity (combinePIIObservations mpo)

return []
tests :: IO Bool
tests = $quickCheckAll
