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
import qualified Data.Text as T

import           P

import           Warden.Data.Field

fromFieldLooks :: FieldLooks -> Value
fromFieldLooks LooksEmpty = "looks-empty"
fromFieldLooks LooksIntegral = "looks-integral"
fromFieldLooks LooksReal = "looks-real"
fromFieldLooks LooksText = "looks-text"
fromFieldLooks LooksCategorical = "looks-categorical"
fromFieldLooks LooksBoolean = "looks-boolean"
fromFieldLooks LooksBroken = "looks-broken"

toFieldLooks :: Value -> Parser FieldLooks
toFieldLooks (String "looks-empty") = pure LooksEmpty
toFieldLooks (String "looks-integral") = pure LooksIntegral
toFieldLooks (String "looks-real") = pure LooksReal
toFieldLooks (String "looks-text") = pure LooksText
toFieldLooks (String "looks-categorical") = pure LooksCategorical
toFieldLooks (String "looks-boolean") = pure LooksBoolean
toFieldLooks (String "looks-broken") = pure LooksBroken
toFieldLooks (String s) = fail . T.unpack $ "invalid field description: " <> s
toFieldLooks x = typeMismatch "Warden.Data.Row.FieldLooks" x

fromFieldType :: FieldType -> Value
fromFieldType TextField = String "text-field"
fromFieldType CategoricalField = String "categorical-field"
fromFieldType BooleanField = String "boolean-field"
fromFieldType IntegralField = String "integral-field"
fromFieldType RealField = String "real-field"

toFieldType :: Value -> Parser FieldType
toFieldType (String "text-field") = pure TextField
toFieldType (String "categorical-field") = pure CategoricalField
toFieldType (String "boolean-field") = pure BooleanField
toFieldType (String "integral-field") = pure IntegralField
toFieldType (String "real-field") = pure RealField
toFieldType (String s) = fail . T.unpack $ "invalid field type: " <> s
toFieldType x = typeMismatch "Warden.Data.Schema.FieldType" x
