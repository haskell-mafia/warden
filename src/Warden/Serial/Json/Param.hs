{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Param(
    fromCheckParams
  , toCheckParams
  , fromWardenVersion
  , toWardenVersion
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T

import           P

import           Warden.Data
import           Warden.Serial.Json.Check
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
