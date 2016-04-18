{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Warden.Parser.PII (
    emailP
  , phoneNumberP
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString (takeWhile1, word8, endOfInput)
import           Data.Attoparsec.ByteString (skipMany, satisfy, choice, count)
import           Data.Char (ord)

import           P hiding (count)

import           Warden.Parser.Common

emailP :: Parser ()
emailP = do
  void $ takeWhile1 (not . (== at))
  void $ word8 at
  void $ takeWhile1 hostPart
  void $ word8 period
  void $ takeWhile1 (not . (== at))
  void $ endOfInput
  where
    at = fromIntegral $ ord '@'

    period = fromIntegral $ ord '.'

    hostPart = not . flip elem [space, period, at]
#ifndef NOINLINE
{-# INLINE emailP #-}
#endif

-- | Matches Australian phone numbers or fully-qualified international phone
-- numbers.
phoneNumberP :: Parser ()
phoneNumberP =
  void $ choice [australianNumberP, internationalNumberP]
#ifndef NOINLINE
{-# INLINE phoneNumberP #-}
#endif

australianNumberP :: Parser ()
australianNumberP = do
  void $ word8 zero
  void $ count 9 phoneCharP
  where
    zero = fromIntegral $ ord '0'
#ifndef NOINLINE
{-# INLINE australianNumberP #-}
#endif

internationalNumberP :: Parser ()
internationalNumberP = do
  void $ word8 plus
  void $ count 11 phoneCharP
  where
    plus = fromIntegral $ ord '+'
#ifndef NOINLINE
{-# INLINE internationalNumberP #-}
#endif

phoneCharP :: Parser ()
phoneCharP = satisfy isPhoneChar >> skipPhoneFiller
  where
    isPhoneChar c = c >= 0x30 && c <= 0x39 -- 0-9
#ifndef NOINLINE
{-# INLINE phoneCharP #-}
#endif

skipPhoneFiller :: Parser ()
skipPhoneFiller = skipMany (satisfy valid)
  where
    valid 0x20 = True -- space
    valid 0x2d = True -- hyphen
    valid 0x2e = True -- period
    valid _ = False
#ifndef NOINLINE
{-# INLINE skipPhoneFiller #-}
#endif
