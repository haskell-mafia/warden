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
import qualified Data.ByteString as BS

import qualified Data.Vector as V

import           P

import           Warden.Data.Row

rawRecordP :: Separator -> ByteString -> RawRecord
rawRecordP (Separator sep) bs = {-# SCC rawRecordP #-}
  (RawRecord . V.fromList) $! BS.split sep bs
{-# INLINE rawRecordP #-}

rawFieldP :: Separator -> Parser ByteString
rawFieldP !(Separator sep) = {-# SCC rawFieldP #-}
  -- Don't need to break on newlines as they're already stripped out.
  takeWhile (/= sep)
{-# INLINE rawFieldP #-}
