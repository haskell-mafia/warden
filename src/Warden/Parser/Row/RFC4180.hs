{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Parser.Row.RFC4180 (
    escapedFieldP
  , rawFieldP
  , rawRecordP
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString (word8, peekWord8, takeWhile)
import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import qualified Data.Vector as V

import           P

import           Warden.Data.Row
import           Warden.Parser.Common

import           X.Data.Attoparsec.ByteString (sepByByte1)

rawRecordP :: Separator -> Parser RawRecord
rawRecordP sep = {-# SCC rawRecordP #-}
  (RawRecord . V.fromList) <$!> rawFieldP sep `sepByByte1` (unSeparator sep)
{-# INLINE rawRecordP #-}

rawFieldP :: Separator -> Parser ByteString
rawFieldP !sep = {-# SCC rawFieldP #-}
  peekWord8 >>= \case
    Just c -> if c == doubleQuote
                then escapedFieldP
                else unescapedFieldP sep
    Nothing -> unescapedFieldP sep
{-# INLINE rawFieldP #-}

-- | We do not unescape the content of escaped fields, as the number of
-- double-quotes present in a text field (as long as it remains consistent)
-- shouldn't affect validation at all.
escapedFieldP :: Parser ByteString
escapedFieldP = {-# SCC escapedFieldP #-} do
  void $ word8 doubleQuote
  -- Balance double quotes, but pass everything else through unprocessed.
  s <- AB.scan False endOfField
  case BS.unsnoc s of
    Nothing ->
      pure ""
    Just (init, last) ->
      if last == doubleQuote
        then pure init
        else pure s
  where
    endOfField st c =
      if c == doubleQuote
        then Just $ not st
        else if st
          then Nothing
          else Just False
    {-# INLINE endOfField #-}
{-# INLINE escapedFieldP #-}

unescapedFieldP :: Separator -> Parser ByteString
unescapedFieldP !(Separator sep) = {-# SCC unescapedFieldP #-}
  takeWhile fieldByte
  where
    -- Don't need to break on newlines as they're already stripped out.
    fieldByte c =
         c /= sep
      && c /= doubleQuote
    {-# INLINE fieldByte #-}
{-# INLINE unescapedFieldP #-}
