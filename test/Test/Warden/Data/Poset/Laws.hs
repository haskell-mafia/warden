{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.Poset.Laws where

import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data.Poset

posetLaws :: (Eq a, Poset a) => (UniquePair a) -> a -> a -> a -> Property
posetLaws p x y z =
  conjoin [
      law_identity x
    , law_equality p
    , law_transitivity x y z
    ]

law_identity :: Poset a => a -> Property
law_identity x =
  (x <=| x) === True

law_equality :: (Eq a, Poset a) => (UniquePair a) -> Property
law_equality (UniquePair x y) =
  (x <=| y && y <=| x) === False

law_transitivity :: Poset a => a -> a -> a -> Property
law_transitivity x y z =
  (x <=| y && y <=| z) ==> (x <=| z)
