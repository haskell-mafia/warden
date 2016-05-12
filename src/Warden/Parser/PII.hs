{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Warden.Parser.PII (
    checkAddress
  , checkEmail
  , checkPhoneNumber
  , streetTypes
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

cBool :: Word8 -> Bool
cBool 0 = False
cBool _ = True

-- | Without any reference to RFC 5321, this parser matches things which look
-- vaguely like they might be email addresses.
checkEmail :: ByteString -> Bool
checkEmail bs = {-# SCC checkEmail #-}
  unsafePerformIO $ do
    BSU.unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) ->
      cBool <$> warden_check_email (castPtr bsPtr) (fromIntegral bsLen)

foreign import ccall unsafe warden_check_email
  :: Ptr Word8
  -> CSize
  -> IO Word8

-- | Matches Australian phone numbers or fully-qualified international phone
-- numbers.
checkPhoneNumber :: ByteString -> Bool
checkPhoneNumber bs = {-# SCC checkPhoneNumber #-}
  unsafePerformIO $ do
    BSU.unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) ->
      cBool <$> warden_check_phone_number (castPtr bsPtr) (fromIntegral bsLen)

foreign import ccall unsafe warden_check_phone_number
  :: Ptr Word8
  -> CSize
  -> IO Word8

-- | Rudimentary address parser for Western-style street-level address
-- prefixes, e.g., 123 Some St or 2/47 Other Road. Does not match some
-- address forms, e.g., "Unit 5, Whatever Road" or "La Maison
-- Bourgeois, 123 Fake St".
--
-- Looks for a number, followed by an alpha string, followed by one of
-- the known street types (road, lane et cetera).
--
-- This parser expects input to be pre-lowercased. It is safe to use
-- 'Warden.Row.Internal.asciiToLower'.
checkAddress :: ByteString -> Bool
checkAddress bs = {-# SCC checkAddress #-}
  unsafePerformIO $ do
    BSU.unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) ->
      cBool <$> warden_check_address (castPtr bsPtr) (fromIntegral bsLen)

foreign import ccall unsafe warden_check_address
  :: Ptr Word8
  -> CSize
  -> IO Word8

streetTypes :: [ByteString]
streetTypes = [
    "st"
  , "rd"
  , "road"
  , "lane"
  , "ln"
  , "cres"
  , "ave"
  ]
{-# INLINE streetTypes #-}
