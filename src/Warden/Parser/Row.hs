{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Parser.Row (
    escapedFieldP
  , fieldP
  , numericFieldP
  , rawFieldP
  , rawRecordP
  , sepByByte1P
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString (word8, peekWord8, takeWhile)
import           Data.Attoparsec.ByteString (string, endOfInput, choice)
import qualified Data.Attoparsec.ByteString as AB
import           Data.Attoparsec.ByteString.Char8 (decimal, signed, double)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char (ord)

import qualified Data.Vector as V

import           P

import           Warden.Data.Numeric
import           Warden.Data.Row
import           Warden.Parser.Common

rawRecordP :: Separator -> Parser RawRecord
rawRecordP sep = (RawRecord . V.fromList) <$!> rawFieldP sep `sepByByte1P` sep
{-# INLINE rawRecordP #-}

rawFieldP :: Separator -> Parser ByteString
rawFieldP !sep =
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
escapedFieldP = do
  void $ word8 doubleQuote
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
{-# INLINE escapedFieldP #-}

unescapedFieldP :: Separator -> Parser ByteString
unescapedFieldP !sep =
  takeWhile fieldByte
  where
    fieldByte c =
         c /= sep'
      && c /= lineFeed
      && c /= carriageReturn
      && c /= doubleQuote

    sep' = unSeparator sep
{-# INLINE unescapedFieldP #-}

fieldP :: Parser ParsedField
fieldP = choice [
    void integralFieldP >> pure ParsedIntegral
  , void realFieldP >> pure ParsedReal
  , void (boolP <* endOfInput) >> pure ParsedBoolean
  ]
{-# INLINE fieldP #-}

integralFieldP :: Parser Integer
integralFieldP = signed (decimal :: Parser Integer) <* endOfInput
{-# INLINE integralFieldP #-}

realFieldP :: Parser Double
realFieldP = double <* endOfInput
{-# INLINE realFieldP #-}

numericFieldP :: Parser NumericField
numericFieldP = choice [
    NumericField <$> realFieldP
  , (NumericField . fromIntegral) <$> integralFieldP
  ]
{-# INLINE numericFieldP #-}

boolP :: Parser ()
boolP = trueP <|> falseP
  where
    trueP = do
      void . word8 . fromIntegral $ ord 't'
      peekWord8 >>= \case
        Nothing -> pure ()
        Just _ -> void $ string "rue"

    falseP = do
      void . word8 . fromIntegral $ ord 'f'
      peekWord8 >>= \case
        Nothing -> pure ()
        Just _ -> void $ string "alse"
{-# INLINE boolP #-}
