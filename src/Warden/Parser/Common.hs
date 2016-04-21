{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Parser.Common (
    carriageReturn
  , sepByByte1P
  , space
  , lineFeed
  , doubleQuote
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString (peekWord8, anyWord8)
import           Data.Char (ord)
import           Data.Word (Word8)

import           P

import           Warden.Data.Row

sepByByte1P :: Parser a -> Separator -> Parser [a]
sepByByte1P p !sep =
  liftM2' (:) p go
  where
    go = do
      peekWord8 >>= \case
        Just c -> if c == sep'
                    then liftM2' (:) (anyWord8 *> p) go
                    else pure []
        Nothing -> pure []

    sep' = unSeparator sep
{-# INLINE sepByByte1P #-}

lineFeed :: Word8
lineFeed = fromIntegral $ ord '\n'
{-# INLINE lineFeed #-}

space :: Word8
space = fromIntegral $ ord ' '
{-# INLINE space #-}

carriageReturn :: Word8
carriageReturn = fromIntegral $ ord '\r'
{-# INLINE carriageReturn #-}

doubleQuote :: Word8
doubleQuote = fromIntegral $ ord '"'
{-# INLINE doubleQuote #-}
