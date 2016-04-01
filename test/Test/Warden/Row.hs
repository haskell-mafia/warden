{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Row where

import           Control.Lens ((^.))

import qualified Data.Set as S

import           Disorder.Core.UniquePair (UniquePair)

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

prop_updateSVParseState_associative :: [ValidRow] -> Property
prop_updateSVParseState_associative xs =
  associativity updateSVParseState initialSVParseState xs id

prop_combineSVParseState_commutative :: UniquePair SVParseState -> Property
prop_combineSVParseState_commutative =
  commutativity combineSVParseState

prop_combineSVParseState_associative :: [SVParseState] -> Property
prop_combineSVParseState_associative xs =
  associativity combineSVParseState initialSVParseState xs id

return []
tests :: IO Bool
tests = $quickCheckAll
