{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Serial.Json.Common (
    fromTextField
  ) where

import           Data.Aeson.Types (Parser)
import qualified Data.Text as T

import           P

fromTextField :: (Text -> Maybe a) -> Text -> Text -> Parser a
fromTextField f typ t = case f t of
  Just x -> pure x
  Nothing -> fail . T.unpack $ T.concat [
      "Invalid "
    , typ
    , ": "
    , t
    ]
