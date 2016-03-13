{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- This module will probably end up in brandix. -}
module Warden.Serial.Json.Field(
    fromFieldLooks
  , toFieldLooks
  , fromFieldType
  , toFieldType
  ) where

import           Data.Aeson.Types (Value(..), Parser, typeMismatch)

import           P

import           Warden.Data.Field
import           Warden.Serial.Json.Common

fromFieldLooks :: FieldLooks -> Value
fromFieldLooks = String . renderFieldLooks

toFieldLooks :: Value -> Parser FieldLooks
toFieldLooks (String s) = fromTextField parseFieldLooks "FieldLooks" s
toFieldLooks x = typeMismatch "Warden.Data.Row.FieldLooks" x

fromFieldType :: FieldType -> Value
fromFieldType = String . renderFieldType

toFieldType :: Value -> Parser FieldType
toFieldType (String s) = fromTextField parseFieldType "FieldType" s
toFieldType x = typeMismatch "Warden.Data.Schema.FieldType" x
