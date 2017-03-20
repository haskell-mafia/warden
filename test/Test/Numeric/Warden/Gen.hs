{-# LANGUAGE NoImplicitPrelude #-}

module Test.Numeric.Warden.Gen(
    genUniformWithin
  ) where

import           Control.Monad.Primitive (PrimBase, PrimState)

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU

import           P

import qualified System.Random.MWC as R

genUniformWithin
  :: PrimBase m
  => Int
  -> Double
  -> Double
  -> R.Gen (PrimState m)
  -> m (Vector Double)
genUniformWithin n lo hi =
  VU.replicateM n . R.uniformR (lo, hi)
