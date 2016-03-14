{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.TextCounts where

import           Data.Text (Text)

import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Warden.Data.TextCounts

prop_hashText :: UniquePair Text -> Property
prop_hashText (UniquePair x y) =
  let x' = hashText x
      y' = hashText y in
  (x' /= y') === True

return []
tests :: IO Bool
tests = $quickCheckAll
