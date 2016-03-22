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
import           Warden.Row

prop_updateSVParseState :: TextFreeformThreshold -> [ValidRow] -> Property
prop_updateSVParseState fft rs =
  let rs' = unValidRow <$> rs
      s = foldl (updateSVParseState fft) initialSVParseState rs' in
  (s ^. badRows, s ^. totalRows) === (RowCount 0, RowCount . fromIntegral $ length rs)

return []
tests :: IO Bool
tests = $quickCheckAll
