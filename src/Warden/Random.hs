{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Random (
    uniformShuffle
  ) where

import           Control.Monad.Primitive (PrimMonad(..))

import qualified Data.Vector.Unboxed.Mutable as MVU

import           P

import           System.IO (IO)
import           System.Random.MWC (Gen, uniformR)

-- | In-place Fisher-Yates shuffle.
uniformShuffle :: (MVU.Unbox a)
               => Gen (PrimState IO)
               -> MVU.IOVector a
               -> IO ()
uniformShuffle g v =
  let n = MVU.length v in
  mapM_ (fisherYates n) [0..(n-2)]
  where
    fisherYates :: Int -> Int -> IO ()
    fisherYates n ix = do
      u <- uniformR (0, n - ix - 1) g
      MVU.swap v ix (ix + u)
