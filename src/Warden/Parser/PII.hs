{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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

-- | Without any reference to RFC 5321, this parser matches things which look
-- vaguely like they might be email addresses.
emailP :: Parser ()
emailP = {-# SCC emailP #-} do
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
{-# INLINE emailP #-}

-- | Matches Australian phone numbers or fully-qualified international phone
-- numbers.
phoneNumberP :: Parser ()
phoneNumberP = {-# SCC phoneNumberP #-}
  void $ choice [australianNumberP, internationalNumberP]
{-# INLINE phoneNumberP #-}

australianNumberP :: Parser ()
australianNumberP = {-# SCC australianNumberP #-} do
  void $ word8 zero
  -- Only match Australian phone numbers with valid area codes.
  void $ secondNum
  void $ count 8 phoneCharP
  where
    zero = fromIntegral $ ord '0'

    secondNum = satisfy isAreaCode >> skipPhoneFiller

    isAreaCode 0x32 = True -- 2, NSW/ACT
    isAreaCode 0x33 = True -- 3, VIC/TAS
    isAreaCode 0x34 = True -- 4, mobiles
    isAreaCode 0x37 = True -- 7, QLD
    isAreaCode 0x38 = True -- 8, SA/WA/NT
    isAreaCode _ = False
{-# INLINE australianNumberP #-}

internationalNumberP :: Parser ()
internationalNumberP = {-# SCC internationalNumberP #-} do
  void $ word8 plus
  void $ count 11 phoneCharP
  where
    plus = fromIntegral $ ord '+'
{-# INLINE internationalNumberP #-}

phoneCharP :: Parser ()
phoneCharP = {-# SCC phoneCharP #-}
  satisfy isPhoneChar >> skipPhoneFiller
  where
    isPhoneChar c = c >= 0x30 && c <= 0x39 -- 0-9
{-# INLINE phoneCharP #-}

skipPhoneFiller :: Parser ()
skipPhoneFiller = {-# SCC skipPhoneFiller #-}
  skipMany (satisfy valid)
  where
    valid 0x20 = True -- space
    valid 0x2d = True -- hyphen
    valid 0x2e = True -- period
    valid _ = False
{-# INLINE skipPhoneFiller #-}
