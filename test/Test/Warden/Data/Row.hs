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

prop_resolveSVParseState :: [SVParseState] -> Property
prop_resolveSVParseState ss =
  let s' = resolveSVParseState ss
      bad' = s' ^. badRows
      total' = s' ^. totalRows
      fns' = S.size $ s' ^. numFields in
  (===) True $ all (\s'' ->    bad' >= (s'' ^. badRows)
                            && total' >= (s'' ^. totalRows)
                            && fns' >= (S.size $ s'' ^. numFields)) ss

prop_updateSVParseState :: [ValidRow] -> Property
prop_updateSVParseState rs =
  let rs' = unValidRow <$> rs
      s = foldl updateSVParseState initialSVParseState rs' in
  (s ^. badRows, s ^. totalRows) === (RowCount 0, RowCount $ length rs)

prop_combineFieldLooks :: [FieldLookCount] -> Property
prop_combineFieldLooks ls =
  let l = foldr combineFieldLooks NoFieldLookCount ls
      lSum = sumFLC l
      tSum = sum $ sumFLC <$> ls in
  lSum === tSum

return []
tests :: IO Bool
tests = $quickCheckAll
