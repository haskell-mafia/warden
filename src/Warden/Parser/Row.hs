{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Parser.Row (
    parseRawRecord
  ) where

import           Data.Attoparsec.ByteString (parseOnly)
import           Data.ByteString (ByteString)
import           Data.String (String)

import           P

import           Warden.Data.Row
import           Warden.Data.Schema
import qualified Warden.Parser.Row.DelimitedText as DelimitedText
import qualified Warden.Parser.Row.RFC4180 as RFC4180

parseRawRecord :: FileFormat -> Separator -> ByteString -> Either String RawRecord
parseRawRecord RFC4180 s b =
  parseOnly (RFC4180.rawRecordP s) b
parseRawRecord DelimitedText s b =
  Right $ DelimitedText.rawRecordP s b
