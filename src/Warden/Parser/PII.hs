{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Warden.Parser.PII (
    emailP
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString (takeWhile1, word8, endOfInput)
import           Data.Char (ord)

import           P

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
