{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Row where

import           Control.Lens ((^.))

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data

prop_resolveSVParseState :: [SVParseState] -> Property
prop_resolveSVParseState ss =
  let s' = resolveSVParseState ss
      bad' = s' ^. badRows
      total' = s' ^. totalRows
      fns' = length $ s' ^. numFields in
  (===) True $ all (\s'' ->    bad' >= (s'' ^. badRows)
                            && total' >= (s'' ^. totalRows)
                            && fns' >= (length $ s'' ^. numFields)) ss

return []
tests :: IO Bool
tests = $quickCheckAll
