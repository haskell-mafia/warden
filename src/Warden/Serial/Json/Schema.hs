{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Serial.Json.Schema(
    fromSchema
  , toSchema
  , fromSchemaFile
  , toSchemaFile
  ) where

import           Data.Aeson ((.:), (.=), object, toJSON, parseJSON)
import           Data.Aeson.Types (Value(..), Parser, typeMismatch)
import qualified Data.Text as T
import qualified Data.Vector as V

import           P

import           Warden.Data.Schema
import           Warden.Serial.Json.Field

fromSchema :: Schema -> Value
fromSchema (Schema v fs) = object [
    "fields" .= (fmap fromSchemaField $ V.toList fs)
  , "version" .= fromSchemaVersion v
  ]

toSchema :: Value -> Parser Schema
toSchema (Object o) = do
  v <- toSchemaVersion =<< (o .: "version")
  fs <- fmap V.fromList $ mapM toSchemaField =<< (o .: "fields")
  pure $ Schema v fs
toSchema x = typeMismatch "Warden.Data.Schema.Schema" x

fromSchemaVersion :: SchemaVersion -> Value
fromSchemaVersion SchemaV1 = String "v1"

toSchemaVersion :: Value -> Parser SchemaVersion
toSchemaVersion (String "v1") = pure SchemaV1
toSchemaVersion (String x) = fail . T.unpack $ "unknown schema version " <> x
toSchemaVersion x = typeMismatch "Warden.Data.Schema.SchemaVersion" x

fromSchemaField :: SchemaField -> Value
fromSchemaField (SchemaField t f) = object [
      "field-type" .= fromFieldType t
    , "field-form" .= fromFieldForm f
    ]

toSchemaField :: Value -> Parser SchemaField
toSchemaField (Object o) = do
  t <- toFieldType =<< (o .: "field-type")
  f <- toFieldForm =<< (o .: "field-form")
  pure $ SchemaField t f
toSchemaField x = typeMismatch "Warden.Data.Schema.SchemaField" x

fromSchemaFile :: SchemaFile -> Value
fromSchemaFile (SchemaFile f) = String $ T.pack f

toSchemaFile :: Value -> Parser SchemaFile
toSchemaFile (String s) = pure . SchemaFile $ T.unpack s
toSchemaFile x = typeMismatch "Warden.Data.Schema.SchemaFile" x

fromFieldForm :: FieldForm -> Value
fromFieldForm FreeForm = object [ "form-type" .= String "freeform" ]
fromFieldForm UnknownForm = object [ "form-type" .= String "unknown" ]
fromFieldForm (CategoricalForm c) = object [
    "form-type" .= String "categorical"
  , "uniques" .= toJSON (unFieldUniques c)
  ]

toFieldForm :: Value -> Parser FieldForm
toFieldForm (Object o) =
  o .: "form-type" >>= \case
    String "freeform" -> pure FreeForm
    String "unknown" -> pure UnknownForm
    String "categorical" -> toCategorical'
    x -> fail $ "Invalid FieldForm: " <> show x
  where
    toCategorical' = do
      c <- fmap FieldUniques $ parseJSON =<< (o .: "uniques")
      pure $ CategoricalForm c
toFieldForm x = typeMismatch "Warden.Data.Schema.FieldForm" x
  
