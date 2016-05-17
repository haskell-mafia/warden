{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Warden.Parser.Field (
    checkFieldBool
  , fieldP
  , numericFieldP
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString (word8, peekWord8)
import           Data.Attoparsec.ByteString (string, endOfInput, choice)
import           Data.Attoparsec.ByteString.Char8 (decimal, signed, double)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           Warden.Data.Numeric
import           Warden.Data.Row
import           Warden.Parser.Common

fieldP :: Parser ParsedField
fieldP = {-# SCC fieldP #-} choice [
    void integralFieldP >> pure ParsedIntegral
  , void realFieldP >> pure ParsedReal
  , void (boolP <* endOfInput) >> pure ParsedBoolean
  ]
{-# INLINE fieldP #-}

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

boolP :: Parser ()
boolP = {-# SCC boolP #-}
  trueP <|> falseP
  where
    trueP = do
      void $ word8 0x74 -- t
      peekWord8 >>= \case
        Nothing -> pure ()
        Just _ -> void $ string "rue"

    falseP = do
      void $ word8 0x66 -- f
      peekWord8 >>= \case
        Nothing -> pure ()
        Just _ -> void $ string "alse"
{-# INLINE boolP #-}
