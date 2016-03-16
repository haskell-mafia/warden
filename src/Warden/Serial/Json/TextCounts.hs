{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Serial.Json.TextCounts(
    fromUniqueTextCount
  , toUniqueTextCount
  ) where

import           Data.Aeson (parseJSON, toJSON, (.=), (.:), object)
import           Data.Aeson.Types (Value(..), Parser, typeMismatch)
import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           Warden.Data.TextCounts

fromUniqueTextCount :: UniqueTextCount -> Value
fromUniqueTextCount LooksFreeform = object [ "type" .= String "looks-freeform" ]
fromUniqueTextCount (UniqueTextCount cs) = object [
    "type" .= String "unique-text-count"
  , "uniques" .= (toJSON $ S.toList cs)
  ]

toUniqueTextCount :: Value -> Parser UniqueTextCount
toUniqueTextCount (Object o) = do
  (o .: "type") >>= \case
    String "looks-freeform" -> pure LooksFreeform
    String "unique-text-count" -> toUniqueTextCount'
    String s -> fail . T.unpack $ "Invalid UniqueTextCount type: " <> s
    x -> typeMismatch "Warden.Data.UniqueTextCount.TextCount.type" x
  where
    toUniqueTextCount' =
      counts' =<< (o .: "uniques")

    counts' (Array as) = (pure . UniqueTextCount . S.fromList) =<< (parseJSON (Array as))
    counts' x = typeMismatch "Warden.Data.UniqueTextCount.TextCount.uniques" x
toUniqueTextCount x = typeMismatch "Warden.Data.TextCount.UniqueTextCount" x
