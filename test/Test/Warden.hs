{-# LANGUAGE NoImplicitPrelude #-}

module Test.Warden where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           P

import           Warden.Data

sumFLC :: FieldLookCount -> ObservationCount
sumFLC l = sum . join $ VU.toList <$> (lookArrays l)
  where
    lookArrays l' = case l' of
      NoFieldLookCount -> []
      FieldLookCount v -> V.toList v

