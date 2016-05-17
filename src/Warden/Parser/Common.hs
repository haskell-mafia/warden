{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Parser.Common (
    cBool
  , doubleQuote
  , lineFeed
  , numeric
  , sepByByte1P
  , space
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString (peekWord8, anyWord8)
import           Data.Word (Word8)

import           P

import           Warden.Data.Row

cBool :: Word8 -> Bool
cBool 0 = False
cBool _ = True

sepByByte1P :: Parser a -> Separator -> Parser [a]
sepByByte1P p !sep = {-# SCC sepByByte1P #-}
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
lineFeed = 0x0a
{-# INLINE lineFeed #-}

space :: Word8
space = 0x20
{-# INLINE space #-}

doubleQuote :: Word8
doubleQuote = 0x22
{-# INLINE doubleQuote #-}

numeric :: Word8 -> Bool
numeric c = c >= 0x30 && c <= 0x39
{-# INLINE numeric #-}
