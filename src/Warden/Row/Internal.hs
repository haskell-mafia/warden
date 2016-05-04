{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}
module Warden.Row.Internal (
    asciiToLower
  , asciiToLower_map
  , asciiToLower_map64
  ) where

import           Data.Bits ((.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import           Data.Word (Word8, Word64)

import           Foreign.C.Types (CSize(..))
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (peekByteOff, pokeByteOff)

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafeDupablePerformIO)

-- | We only care about ASCII characters here (true, false et cetera)
-- and converting unicode to lowercase is really expensive, so just
-- bitwise-or with the case bit (2^5).
--
-- This will bork some characters (higher-range punctuation), but they're not
-- digits or bools so we don't care about them.
asciiToLower :: ByteString -> ByteString
asciiToLower bs = {-# SCC asciiToLower #-}
  BSI.unsafeCreate (BS.length bs) $ \bufPtr ->
    BSU.unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) ->
      warden_ascii_to_lower (fromIntegral bsLen) (castPtr bufPtr) (castPtr bsPtr)
{-# INLINABLE asciiToLower #-}

asciiToLower_map :: ByteString -> ByteString
asciiToLower_map bs = {-# SCC asciiToLower_map #-}
  BS.map ((.|.) 0x20) bs
{-# INLINABLE asciiToLower_map #-}

asciiToLower_map64 :: ByteString -> ByteString
asciiToLower_map64 bs = {-# SCC asciiToLower_map64 #-}
  map64 ((.|.) 0x2020202020202020) ((.|.) 0x20) bs
{-# INLINABLE asciiToLower_map64 #-}

map64 :: (Word64 -> Word64) -> (Word8 -> Word8) -> ByteString -> ByteString
map64 f64 f8 (PS fp s len) =
  let
    map_ :: Int -> Ptr Word8 -> Ptr Word8 -> IO ()
    map_ !n !p1 !p2 =
      let
        !n16 = n + 16
        !n8  = n + 8
        !n1  = n + 1
      in
        if n16 <= len then do
          !x0 <- peekByteOff p1 n
          pokeByteOff p2 n (f64 x0)
          !x1 <- peekByteOff p1 n8
          pokeByteOff p2 n8 (f64 x1)
          map_ n16 p1 p2
        else if n1 <= len then do
          !x <- peekByteOff p1 n
          pokeByteOff p2 n (f8 x)
          map_ n1 p1 p2
        else
          return ()
  in
    unsafeDupablePerformIO $ withForeignPtr fp $ \a ->
      BSI.create len $ map_ 0 (a `plusPtr` s)
{-# INLINE map64 #-}

foreign import ccall unsafe warden_ascii_to_lower
  :: CSize
  -> Ptr Word8
  -> Ptr Word8
  -> IO ()
