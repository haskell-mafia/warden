{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Parser.Row.DelimitedText (
    rawFieldP
  , rawRecordP
  ) where

import           Data.Attoparsec.ByteString (Parser, takeWhile)
import           Data.ByteString (ByteString)

import qualified Data.Vector as V

import           P

import           Warden.Data.Row
import           Warden.Parser.Common

rawRecordP :: Separator -> Parser RawRecord
rawRecordP sep = {-# SCC rawRecordP #-}
  (RawRecord . V.fromList) <$!> rawFieldP sep `sepByByte1P` sep
{-# INLINE rawRecordP #-}

rawFieldP :: Separator -> Parser ByteString
rawFieldP !(Separator sep) = {-# SCC rawFieldP #-}
  -- Don't need to break on newlines as they're already stripped out.
  takeWhile (/= sep)
{-# INLINE rawFieldP #-}
