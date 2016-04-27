{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Parser.Row (
    parserFor
  ) where

import           Data.Attoparsec.ByteString (Parser)

import           Warden.Data.Row
import           Warden.Data.Schema
import qualified Warden.Parser.Row.DelimitedText as DelimitedText
import qualified Warden.Parser.Row.RFC4180 as RFC4180

parserFor :: FileFormat -> Separator -> Parser RawRecord
parserFor RFC4180 = RFC4180.rawRecordP
parserFor DelimitedText = DelimitedText.rawRecordP
