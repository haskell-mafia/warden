{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Row where

import           Control.Lens ((^.))

import qualified Data.Set as S

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden
import           Test.Warden.Arbitrary

import           Warden.Data

prop_resolveSVParseState :: TextFreeformThreshold -> [SVParseState] -> Property
prop_resolveSVParseState fft ss =
  let s' = resolveSVParseState fft ss
      bad' = s' ^. badRows
      total' = s' ^. totalRows
      fns' = S.size $ s' ^. numFields in
  (===) True $ all (\s'' ->    bad' >= (s'' ^. badRows)
                            && total' >= (s'' ^. totalRows)
                            && fns' >= (S.size $ s'' ^. numFields)) ss

prop_updateSVParseState :: TextFreeformThreshold -> [ValidRow] -> Property
prop_updateSVParseState fft rs =
  let rs' = unValidRow <$> rs
      s = foldl (updateSVParseState fft) initialSVParseState rs' in
  (s ^. badRows, s ^. totalRows) === (RowCount 0, RowCount . fromIntegral $ length rs)

prop_combineFieldLooks :: [FieldLookCount] -> Property
prop_combineFieldLooks ls =
  let l = foldr combineFieldLooks NoFieldLookCount ls
      lSum = sumFLC l
      tSum = sum $ sumFLC <$> ls in
  lSum === tSum

return []
tests :: IO Bool
tests = $quickCheckAll
