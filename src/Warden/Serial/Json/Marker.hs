{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Marker(
    fromFileMarker
  , toFileMarker
  ) where

import           Data.Aeson ((.:), (.=), object)
import           Data.Aeson.Types (Value(..), Parser, typeMismatch)
import           Data.Text (Text)
import qualified Data.Text as T

import           Delorean.Local.DateTime (DateTime, renderDateTime, parseDateTime)

import           P

import           Warden.Data
import           Warden.Serial.Json.Check
import           Warden.Serial.Json.View

fromMarkerVersion :: MarkerVersion -> Value
fromMarkerVersion V1 = String "v1"

toMarkerVersion :: Value -> Parser MarkerVersion
toMarkerVersion (String s) = case s of
  "v1" -> pure V1
  _    -> fail $ "Unknown marker version " <> (T.unpack s)
toMarkerVersion x          = typeMismatch "Warden.Data.Marker.MarkerVersion" x

fromDateTime :: DateTime -> Value
fromDateTime = String . renderDateTime

toDateTime :: Value -> Parser DateTime
toDateTime (String s) = either fail pure $ parseDateTime s
toDateTime x          = typeMismatch "Delorean.Local.DateTime.DateTime" x

fromCheckResultType :: CheckResultType -> Value
fromCheckResultType FileResult = String "file"
fromCheckResultType RowResult = String "row"

toCheckResultType :: Value -> Parser CheckResultType
toCheckResultType (String r) = case r of
  "file" -> pure FileResult
  "row"  -> pure RowResult
  x      -> fail $ "Invalid check result type " <> T.unpack x
toCheckResultType x          = typeMismatch "Warden.Data.Marker.CheckResultType" x

fromMarkerFailure :: MarkerFailure -> Value
fromMarkerFailure (MarkerFailure f) = String f

toMarkerFailure :: Value -> Parser MarkerFailure
toMarkerFailure (String s) = pure $ MarkerFailure s
toMarkerFailure x          = typeMismatch "Warden.Data.Marker.MarkerFailure" x

fromMarkerStatus :: MarkerStatus -> Value
fromMarkerStatus MarkerPass = object [
    "status" .= String "pass"
  ]
fromMarkerStatus (MarkerFail f) = object [
    "status" .= String "fail"
  , "failure" .= fromMarkerFailure f
  ]

toMarkerStatus :: Value -> Parser MarkerStatus
toMarkerStatus (Object o) = do
  st <- o .: "status"
  case (st :: Text) of
    "pass" -> pure MarkerPass
    "fail" -> do
      f <- toMarkerFailure =<< (o .: "failure")
      pure $ MarkerFail f
    x      -> fail $ "Invalid marker status: " <> T.unpack x
toMarkerStatus x          = typeMismatch "Warden.Data.Marker.MarkerStatus" x

fromCheckResultSummary :: CheckResultSummary -> Value
fromCheckResultSummary (CheckResultSummary s d t) = object [
    "status" .= fromMarkerStatus s
  , "description" .= fromCheckDescription d
  , "type" .= fromCheckResultType t
  ]

toCheckResultSummary :: Value -> Parser CheckResultSummary
toCheckResultSummary (Object o) = do
  s <- toMarkerStatus =<< (o .: "status")
  d <- toCheckDescription =<< (o .: "description")
  t <- toCheckResultType =<< (o .: "type")
  pure $ CheckResultSummary s d t
toCheckResultSummary x          = typeMismatch "Warden.Data.Marker.CheckResultSummary" x

fromFileMarker :: FileMarker -> Value
fromFileMarker (FileMarker v vf ts crs) = object [
    "version" .= fromMarkerVersion v
  , "view-file" .= fromViewFile vf
  , "timestamp" .= fromDateTime ts
  , "results" .= (fromCheckResultSummary <$> crs)
  ]

toFileMarker :: Value -> Parser FileMarker
toFileMarker (Object o) = do
  v <- toMarkerVersion =<< (o .: "version")
  vf <- toViewFile =<< (o .: "view-file")
  ts <- toDateTime =<< (o .: "timestamp")
  crs <- mapM toCheckResultSummary =<< (o .: "results")
  pure $ FileMarker v vf ts crs
toFileMarker x          = typeMismatch "Warden.Data.Marker.FileMarker" x

