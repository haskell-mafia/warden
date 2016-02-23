{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Schema(
    fromSchema
  , toSchema
  ) where

import           Data.Aeson ((.:), (.=), object)
import           Data.Aeson.Types (Value(..), Parser, typeMismatch)
import qualified Data.Text as T
import qualified Data.Vector as V

import           P

import           Warden.Data.Schema
import           Warden.Serial.Json.Row

fromSchema :: Schema -> Value
fromSchema (Schema c fs) = object [
    "field-count" .= fromFieldCount c
  , "fields" .= (fmap fromSchemaField $ V.toList fs)
  ]

toSchema :: Value -> Parser Schema
toSchema (Object o) = do
  c <- toFieldCount =<< (o .: "field-count")
  fs <- fmap V.fromList $ mapM toSchemaField =<< (o .: "fields")
  pure $ Schema c fs
toSchema x = typeMismatch "Warden.Data.Schema.Schema" x

fromSchemaField :: SchemaField -> Value
fromSchemaField (SchemaField t) = object [
      "field-type" .= fromFieldType t
    ]

toSchemaField :: Value -> Parser SchemaField
toSchemaField (Object o) = do
  f <- toFieldType =<< (o .: "field-type")
  pure $ SchemaField f
toSchemaField x = typeMismatch "Warden.Data.Schema.SchemaField" x

fromFieldType :: FieldType -> Value
fromFieldType TextField = String "text-field"
fromFieldType CategoricalField = String "categorical-field"
fromFieldType IntegralField = String "integral-field"
fromFieldType RealField = String "real-field"

toFieldType :: Value -> Parser FieldType
toFieldType (String "text-field") = pure TextField
toFieldType (String "categorical-field") = pure CategoricalField
toFieldType (String "integral-field") = pure IntegralField
toFieldType (String "real-field") = pure RealField
toFieldType (String s) = fail . T.unpack $ "invalid field type: " <> s
toFieldType x = typeMismatch "Warden.Data.Schema.FieldType" x

