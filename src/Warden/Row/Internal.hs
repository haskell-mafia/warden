{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Warden.Row.Internal (
    asciiToLower
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C

import           P

import           System.IO (IO)

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
{-# INLINE asciiToLower #-}

foreign import ccall unsafe warden_ascii_to_lower
  :: CSize
  -> Ptr Word8
  -> Ptr Word8
  -> IO ()
