{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Row.Parser (
    rawRecordP
  ) where

import           Data.Attoparsec.Text (Parser)
import           Data.Attoparsec.Text (char, peekChar, takeWhile, anyChar)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text as T

import qualified Data.Vector as V

import           P

import           Warden.Data.Row

rawRecordP :: Separator -> Parser RawRecord
rawRecordP sep = (RawRecord . V.fromList) <$!> rawFieldP sep `sepByChar1P` sep
{-# INLINE rawRecordP #-}

sepByChar1P :: Parser a -> Separator -> Parser [a]
sepByChar1P p !sep =
  liftM2' (:) p go
  where
    go = do
      peekChar >>= \case
        Just c -> if c == sep'
                    then liftM2' (:) (anyChar *> p) go
                    else pure []
        Nothing -> pure []

    sep' = separatorToChar sep
{-# INLINE sepByChar1P #-}
      
rawFieldP :: Separator -> Parser Text
rawFieldP !sep =
  peekChar >>= \case
    Just c -> if c == '"'
                then escapedFieldP
                else unescapedFieldP sep
    Nothing -> unescapedFieldP sep
{-# INLINE rawFieldP #-}

-- | We do not unescape the content of escaped fields, as the number of
-- double-quotes present in a text field (as long as it remains consistent)
-- shouldn't affect validation at all.
escapedFieldP :: Parser Text
escapedFieldP =
  char '"' *> (T.init <$> (AT.scan False endOfField))
  where
    endOfField st '"' = Just $ not st
    endOfField True _ = Nothing
    endOfField False _ = Just False
{-# INLINE escapedFieldP #-}

unescapedFieldP :: Separator -> Parser Text
unescapedFieldP !sep =
  takeWhile fieldChar
  where
    fieldChar c =
         c /= sep'
      && c /= '\n'
      && c /= '\r'
      && c /= '"'

    sep' = separatorToChar sep
{-# INLINE unescapedFieldP #-}
