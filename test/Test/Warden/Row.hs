{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Row where

import           Control.Lens ((^.))

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Row

prop_updateSVParseState :: TextFreeformThreshold -> [ValidRow] -> Property
prop_updateSVParseState fft rs =
  let rs' = unValidRow <$> rs
      s = foldl (updateSVParseState fft) initialSVParseState rs' in
  (s ^. badRows, s ^. totalRows) === (RowCount 0, RowCount . fromIntegral $ length rs)

prop_resolveSVParseState :: TextFreeformThreshold -> [SVParseState] -> Property
prop_resolveSVParseState fft ss =
  let s' = resolveSVParseState fft ss
      bad' = s' ^. badRows
      total' = s' ^. totalRows
      fns' = S.size $ s' ^. numFields in
  (===) True $ all (\s'' ->    bad' >= (s'' ^. badRows)
                            && total' >= (s'' ^. totalRows)
                            && fns' >= (S.size $ s'' ^. numFields)) ss

prop_updateFieldNumericState' :: Int -> Double -> Property
prop_updateFieldNumericState' m n =
  let nb = T.encodeUtf8 $ renderFractional n
      mb = T.encodeUtf8 $ renderIntegral m
      ns = updateFieldNumericState' nb initialNumericState
      ms = updateFieldNumericState' mb initialNumericState in
  (ns == initialNumericState, ms == initialNumericState) === (False, False)

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

prop_parseField :: TestField -> Property
prop_parseField tf =
  let bs = packTestField tf
      fl = parseField bs in
  case tf of
    TestIntegral _ -> fl === LooksIntegral
    TestReal _ -> fl === LooksReal
    TestText _ -> fl === LooksText
    TestBoolean _ -> fl === LooksBoolean

return []
tests :: IO Bool
tests = $quickCheckAll
