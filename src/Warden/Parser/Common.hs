{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

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
#ifndef NOINLINE
{-# INLINE sepByByte1P #-}
#endif

lineFeed :: Word8
lineFeed = fromIntegral $ ord '\n'
#ifndef NOINLINE
{-# INLINE lineFeed #-}
#endif

space :: Word8
space = fromIntegral $ ord ' '
#ifndef NOINLINE
{-# INLINE space #-}
#endif

carriageReturn :: Word8
carriageReturn = fromIntegral $ ord '\r'
#ifndef NOINLINE
{-# INLINE carriageReturn #-}
#endif

doubleQuote :: Word8
doubleQuote = fromIntegral $ ord '"'
#ifndef NOINLINE
{-# INLINE doubleQuote #-}
#endif
