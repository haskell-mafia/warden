{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Row where

import           Control.Lens ((^.))

import qualified Data.Set as S

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden
import           Test.Warden.Arbitrary ()

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

prop_combineFieldLooks :: [FieldLookCount] -> Property
prop_combineFieldLooks ls =
  let l = foldr combineFieldLooks NoFieldLookCount ls
      lSum = sumFLC l
      tSum = sum $ sumFLC <$> ls in
  lSum === tSum

return []
tests :: IO Bool
tests = $quickCheckAll
