{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

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
import           Warden.Serial.Json.TextCounts

fromIncludeDotFiles :: IncludeDotFiles -> Value
fromIncludeDotFiles IncludeDotFiles = String "include-dot-files"
fromIncludeDotFiles NoIncludeDotFiles = String "no-include-dot-files"

toIncludeDotFiles :: Value -> Parser IncludeDotFiles
toIncludeDotFiles (String "include-dot-files") = pure IncludeDotFiles
toIncludeDotFiles (String "no-include-dot-files") = pure NoIncludeDotFiles
toIncludeDotFiles (String s) = fail . T.unpack $ "invalid IncludeDotFiles: " <> s
toIncludeDotFiles x = typeMismatch "Warden.Data.Param.IncludeDotFiles" x

fromForce :: Force -> Value
fromForce Force = String "force"
fromForce NoForce = String "no-force"

toForce :: Value -> Parser Force
toForce (String "force") = pure Force
toForce (String "no-force") = pure NoForce
toForce (String s) = fail $ "invalid Force parameter: " <> T.unpack s
toForce x = typeMismatch "Warden.Data.Param.Force" x

fromExitType :: ExitType -> Value
fromExitType ExitWithCheckStatus = "check-status-exit"
fromExitType ExitWithSuccess = "success-exit"

toExitType :: Value -> Parser ExitType
toExitType (String "check-status-exit") = pure ExitWithCheckStatus
toExitType (String "success-exit") = pure ExitWithSuccess
toExitType (String s) = fail $ "invalid ExitType parameter: " <> T.unpack s
toExitType x = typeMismatch "Warden.Data.Param.ExitType" x

fromSamplingType :: SamplingType -> Value
fromSamplingType NoSampling = object [
    "type" .= String "no-sampling"
  ]
fromSamplingType (ReservoirSampling rs) = object [
    "type" .= String "reservoir-sampling"
  , "reservoir-size" .= fromReservoirSize rs
  ]

toSamplingType :: Value -> Parser SamplingType
toSamplingType (Object o) = do
  o .: "type" >>= \case
    String "no-sampling" -> pure NoSampling
    String "reservoir-sampling" -> do
      rs <- toReservoirSize =<< (o .: "reservoir-size")
      pure $ ReservoirSampling rs
    String s -> fail . T.unpack $ "Invalid sampling type: " <> s
    x -> typeMismatch "Warden.Data.Param.SamplingType.type" x
toSamplingType x = typeMismatch "Warden.Data.Param.SamplingType" x

fromReservoirSize :: ReservoirSize -> Value
fromReservoirSize = toJSON . unReservoirSize

toReservoirSize :: Value -> Parser ReservoirSize
toReservoirSize (Number n) = fmap ReservoirSize $ parseJSON (Number n)
toReservoirSize x = typeMismatch "Warden.Data.Sampling.Reservoir.ReservoirSize" x

fromCheckParams :: CheckParams -> Value
fromCheckParams (CheckParams sep sf lb verb fce fft xt idf st) = object [
    "separator" .= fromSeparator sep
  , "line-bound" .= fromLineBound lb
  , "verbosity" .= fromVerbosity verb
  , "force" .= fromForce fce
  , "schema-file" .= maybe Null fromSchemaFile sf
  , "freeform-text-threshold" .= fromTextFreeformThreshold fft
  , "exit-type" .= fromExitType xt
  , "include-dot-files" .= fromIncludeDotFiles idf
  , "sampling-type" .= fromSamplingType st
  ]

toCheckParams :: Value -> Parser CheckParams
toCheckParams (Object o) = do
  sep <- toSeparator =<< (o .: "separator")
  lb <- toLineBound =<< (o .: "line-bound")
  verb <- toVerbosity =<< (o .: "verbosity")
  fce <- toForce =<< (o .: "force")
  sf <- maybe (pure Nothing) (fmap Just . toSchemaFile) =<< (o .:? "schema-file")
  fft <- toTextFreeformThreshold =<< (o .: "freeform-text-threshold")
  xt <- toExitType =<< (o .: "exit-type")
  idf <- toIncludeDotFiles =<< (o .: "include-dot-files")
  st <- maybe (pure NoSampling) toSamplingType =<< (o .:? "sampling-type")
  pure $ CheckParams sep sf lb verb fce fft xt idf st
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
