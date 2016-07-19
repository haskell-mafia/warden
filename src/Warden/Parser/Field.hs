{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Warden.Parser.Field (
    checkFieldBool
  , checkFieldDate
  , checkFieldNumeric
  , integralFieldP
  , realFieldP
  , numericFieldP
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString (endOfInput, choice)
import           Data.Attoparsec.ByteString.Char8 (decimal, signed, double)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           Warden.Data.Numeric
import           Warden.Data.Field
import           Warden.Parser.Common

checkFieldBool :: ByteString -> Bool
checkFieldBool bs = {-# SCC checkFieldBool #-}
  unsafePerformIO $ do
    BSU.unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) ->
      cBool <$> warden_field_bool (castPtr bsPtr) (fromIntegral bsLen)
{-# INLINE checkFieldBool #-}

foreign import ccall unsafe warden_field_bool
  :: Ptr Word8
  -> CSize
  -> IO Word8

-- | Tests for the presence of an integral/real field without storing the
-- value. Faster than 'integralFieldP' and 'realFieldP'.
checkFieldNumeric :: ByteString -> Maybe' FieldLooks
checkFieldNumeric bs = {-# SCC checkFieldNumeric #-}
  unsafePerformIO $ do
    BSU.unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) ->
      cNumericField <$> warden_field_numeric (castPtr bsPtr) (fromIntegral bsLen)
  where
    cNumericField 1 = Just' LooksIntegral
    cNumericField 2 = Just' LooksReal
    cNumericField _ = Nothing'
    {-# INLINE cNumericField #-}
{-# INLINE checkFieldNumeric #-}

foreign import ccall unsafe warden_field_numeric
  :: Ptr Word8
  -> CSize
  -> IO Word8

integralFieldP :: Parser Integer
integralFieldP = {-# SCC integralFieldP #-}
  signed (decimal :: Parser Integer) <* endOfInput
{-# INLINE integralFieldP #-}

realFieldP :: Parser Double
realFieldP = {-# SCC realFieldP #-}
  double <* endOfInput
{-# INLINE realFieldP #-}

numericFieldP :: Parser NumericField
numericFieldP = {-# SCC numericFieldP #-}
  choice [
    NumericField <$> realFieldP
  , (NumericField . fromIntegral) <$> integralFieldP
  ]
{-# INLINE numericFieldP #-}

checkFieldDate :: ByteString -> Bool
checkFieldDate bs = {-# SCC checkFieldDate #-}
  unsafePerformIO $ do
    BSU.unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) ->
      cBool <$> warden_field_datetime (castPtr bsPtr) (fromIntegral bsLen)
{-# INLINE checkFieldDate #-}

foreign import ccall unsafe warden_field_datetime
  :: Ptr Word8
  -> CSize
  -> IO Word8
