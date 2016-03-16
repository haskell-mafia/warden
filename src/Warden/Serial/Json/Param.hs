{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Param(
    fromCheckParams
  , toCheckParams
  , fromWardenParams
  , toWardenParams
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T

import           P

import           Warden.Data
import           Warden.Serial.Json.Common
import           Warden.Serial.Json.Row
import           Warden.Serial.Json.Schema

fromForce :: Force -> Value
fromForce Force = String "force"
fromForce NoForce = String "no-force"

toForce :: Value -> Parser Force
toForce (String "force") = pure Force
toForce (String "no-force") = pure NoForce
toForce (String s) = fail $ "invalid Force parameter: " <> T.unpack s
toForce x = typeMismatch "Warden.Data.Param.Force" x

fromCheckParams :: CheckParams -> Value
fromCheckParams (CheckParams sep sf lb verb fce) = object [
    "separator" .= fromSeparator sep
  , "line-bound" .= fromLineBound lb
  , "verbosity" .= fromVerbosity verb
  , "force" .= fromForce fce
  , "schema-file" .= maybe Null fromSchemaFile sf
  ]

toCheckParams :: Value -> Parser CheckParams
toCheckParams (Object o) = do
  sep <- toSeparator =<< (o .: "separator")
  lb <- toLineBound =<< (o .: "line-bound")
  verb <- toVerbosity =<< (o .: "verbosity")
  fce <- toForce =<< (o .: "force")
  sf <- maybe (pure Nothing) (fmap Just . toSchemaFile) =<< (o .:? "schema-file")
  pure $ CheckParams sep sf lb verb fce
toCheckParams x = typeMismatch "Warden.Data.Param.CheckParams" x

fromWardenVersion :: WardenVersion -> Value
fromWardenVersion (WardenVersion v) = String v

toWardenVersion :: Value -> Parser WardenVersion
toWardenVersion (String s) = pure $ WardenVersion s
toWardenVersion x = typeMismatch "Warden.Data.Params.WardenVersion" x

fromRunId :: RunId -> Value
fromRunId = String . renderRunId

toRunId :: Value -> Parser RunId
toRunId (String s) = case parseRunId s of
  Just rid -> pure rid
  Nothing -> fail . T.unpack $ "invalid RunId: " <> s
toRunId x = typeMismatch "Warden.Data.Params.RunId" x

fromNumCPUs :: NumCPUs -> Value
fromNumCPUs = toJSON . unNumCPUs

toNumCPUs :: Value -> Parser NumCPUs
toNumCPUs (Number n) = fmap NumCPUs . parseJSON $ Number n
toNumCPUs x = typeMismatch "Warden.Data.Param.NumCPUs" x


fromVerbosity :: Verbosity -> Value
fromVerbosity = String . renderVerbosity

toVerbosity :: Value -> Parser Verbosity
toVerbosity (String s) = fromTextField parseVerbosity "Verbosity" s
toVerbosity x = typeMismatch "Warden.Data.Check.Verbosity" x

fromWardenParams :: WardenParams -> Value
fromWardenParams (WardenParams caps wv rid) = object [
    "warden-version" .= fromWardenVersion wv
  , "num-caps" .= fromNumCPUs caps
  , "run-id" .= fromRunId rid
  ]

toWardenParams :: Value -> Parser WardenParams
toWardenParams (Object o) = do
  wv <- toWardenVersion =<< (o .: "warden-version")
  caps <- toNumCPUs =<< (o .: "num-caps")
  rid <- toRunId =<< (o .: "run-id")
  pure $ WardenParams caps wv rid
toWardenParams x = typeMismatch "Warden.Data.Param.WardenParams" x
