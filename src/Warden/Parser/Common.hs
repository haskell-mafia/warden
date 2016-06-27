{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Parser.Common (
    cBool
  , doubleQuote
  , lineFeed
  , numeric
  , space
  ) where

import           Data.Word (Word8)

import           P

cBool :: Word8 -> Bool
cBool 0 = False
cBool _ = True
{-# INLINE cBool #-}

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
