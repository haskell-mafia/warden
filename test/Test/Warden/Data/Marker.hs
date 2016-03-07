{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Marker where

import qualified Data.Set as S

import           Disorder.Core (tripping)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data

prop_tripping_filemarker :: ViewFile -> Property
prop_tripping_filemarker =
  tripping fileToMarker markerToFile

prop_dateRange :: ViewMarker -> Property
prop_dateRange vm =
  let dates = vmDates $ vmMetadata vm
      dr = dateRange dates in
  case dr of
    NoDates -> S.size dates === 0
    DateRange a b -> (a <= b) === True

return []
tests :: IO Bool
tests = $quickCheckAll
