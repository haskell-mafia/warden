{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Serial.Json.Marker(
    fromFileMarker
  , toFileMarker
  , fromRowCountSummary
  , toRowCountSummary
  , fromViewMarker
  , toViewMarker
  ) where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Aeson ((.:), (.:?), (.=), object, toJSON, parseJSON)
import           Data.Aeson.Types (Value(..), Parser, typeMismatch)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import           Delorean.Local.Date (Date, renderDate, parseDate)
import           Delorean.Local.DateTime (DateTime, renderDateTime, parseDateTime)

import           P

import           Warden.Data
import           Warden.Serial.Json.Check
import           Warden.Serial.Json.Numeric
import           Warden.Serial.Json.Param
import           Warden.Serial.Json.Row
import           Warden.Serial.Json.TextCounts
import           Warden.Serial.Json.View

data MarkerVersion =
    MarkerV1
  | MarkerV2
  | MarkerV3
  | MarkerV4
  deriving (Eq, Show, Ord, Bounded, Enum)

currentMarkerVersion :: MarkerVersion
currentMarkerVersion = maxBound

fromNonEmpty :: ToJSON a => NonEmpty a -> Value
fromNonEmpty = toJSON . NE.toList

toNonEmpty :: FromJSON a => Value -> Parser (NonEmpty a)
toNonEmpty (Array xs) = do
  parseJSON (Array xs) >>= \case
    [] -> fail "non-empty list cannot be empty"
    xs' -> pure $ NE.fromList xs'
toNonEmpty x          = typeMismatch "Data.List.NonEmpty.NonEmpty" x

fromMarkerVersion :: MarkerVersion -> Value
fromMarkerVersion MarkerV1 = String "v1"
fromMarkerVersion MarkerV2 = String "v2"
fromMarkerVersion MarkerV3 = String "v3"
fromMarkerVersion MarkerV4 = String "v4"

toMarkerVersion :: Value -> Parser MarkerVersion
toMarkerVersion (String s) = case s of
  "v1" -> pure MarkerV1
  "v2" -> pure MarkerV2
  "v3" -> pure MarkerV3
  "v4" -> pure MarkerV4
  _    -> fail $ "Unknown marker version " <> (T.unpack s)
toMarkerVersion x          = typeMismatch "Warden.Data.Marker.MarkerVersion" x

fromDate :: Date -> Value
fromDate = String . renderDate

toDate :: Value -> Parser Date
toDate (String s) = either fail pure $ parseDate s
toDate x = typeMismatch "Delorean.Local.Date.Date" x

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
fromMarkerFailure (MarkerFailure fs) = fromNonEmpty fs

toMarkerFailure :: Value -> Parser MarkerFailure
toMarkerFailure (Array fs) = MarkerFailure <$> toNonEmpty (Array fs)
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
fromFileMarker (FileMarker wps vf ts crs) = object [
    "version" .= fromMarkerVersion currentMarkerVersion
  , "warden-params" .= fromWardenParams wps
  , "view-file" .= fromViewFile vf
  , "timestamp" .= fromDateTime ts
  , "results" .= (fromCheckResultSummary <$> crs)
  ]

toFileMarker :: Value -> Parser FileMarker
toFileMarker (Object o) = do
  (toMarkerVersion =<< (o .: "version")) >>= \case
    MarkerV1 -> markerV1V4
    MarkerV2 -> markerV1V4
    MarkerV3 -> markerV1V4
    MarkerV4 -> markerV1V4
  where
    markerV1V4 = do
      wps <- toWardenParams =<< (o .: "warden-params")
      vf <- toViewFile =<< (o .: "view-file")
      ts <- toDateTime =<< (o .: "timestamp")
      crs <- mapM toCheckResultSummary =<< (o .: "results")
      pure $ FileMarker wps vf ts crs
toFileMarker x          = typeMismatch "Warden.Data.Marker.FileMarker" x

fromViewMetadata :: ViewMetadata -> Value
fromViewMetadata (ViewMetadata rcs ps ds vfs) = object [
    "counts" .= fromRowCountSummary rcs
  , "check-params" .= fromCheckParams ps
  , "check-dates" .= (fmap fromDate $ S.toList ds)
  , "check-files" .= (fmap fromViewFile $ S.toList vfs)
  ]

toViewMetadata :: Value -> Parser ViewMetadata
toViewMetadata (Object o) = do
  rcs <- toRowCountSummary =<< (o .: "counts")
  ps <- toCheckParams =<< (o .: "check-params")
  ds <- fmap S.fromList $ mapM toDate =<< (o .: "check-dates")
  vfs <- fmap S.fromList $ mapM toViewFile =<< (o .: "check-files")
  pure $ ViewMetadata rcs ps ds vfs
toViewMetadata x          = typeMismatch "Warden.Data.Marker.ViewMetadata" x

fromViewMarker :: ViewMarker -> Value
fromViewMarker (ViewMarker wps vi ts crs vm) = object [
    "version" .= fromMarkerVersion currentMarkerVersion
  , "warden-params" .= fromWardenParams wps
  , "view" .= fromView vi
  , "timestamp" .= fromDateTime ts
  , "results" .= (fromCheckResultSummary <$> crs)
  , "metadata" .= fromViewMetadata vm
  ]

toViewMarker :: Value -> Parser ViewMarker
toViewMarker (Object o) = do
  (toMarkerVersion =<< (o .: "version")) >>= \case
    MarkerV1 -> markerV1V4
    MarkerV2 -> markerV1V4
    MarkerV3 -> markerV1V4
    MarkerV4 -> markerV1V4
  where
    markerV1V4 = do
      wps <- toWardenParams =<< (o .: "warden-params")
      vi <- toView =<< (o .: "view")
      ts <- toDateTime =<< (o .: "timestamp")
      crs <- mapM toCheckResultSummary =<< (o .: "results")
      vm <- toViewMetadata =<< (o .: "metadata")
      pure $ ViewMarker wps vi ts crs vm
toViewMarker x          = typeMismatch "Warden.Data.Marker.ViewMarker" x

fromRowCountSummary :: RowCountSummary -> Value
fromRowCountSummary (RowCountSummary br tr nfs fas tcs nss) = object $ [
    "bad-rows" .= fromRowCount br
  , "total-rows" .= fromRowCount tr
  , "field-counts" .= (fmap fromFieldCount $ S.toList nfs)
  , "numeric-summaries" .= fromNumericFieldSummary nss
  ] <> fieldLooks' <> textCounts'
  where
    fieldLooks' = case fas of
      NoFieldLookCount ->
        []
      FieldLookCount fas' ->
        ["field-looks" .= (fmap fromFieldVector $ V.toList fas')]

    textCounts' = case tcs of
      NoTextCounts ->
        []
      TextCounts tcs' ->
        ["text-counts" .= (fmap fromUniqueTextCount $ V.toList tcs')]

toRowCountSummary :: Value -> Parser RowCountSummary
toRowCountSummary (Object o) = do
  br <- toRowCount =<< (o .: "bad-rows")
  tr <- toRowCount =<< (o .: "total-rows")
  nfs <- fmap S.fromList $ mapM toFieldCount =<< (o .: "field-counts")
  fas <- maybe (pure NoFieldLookCount) (fmap FieldLookCount . mapM toFieldVector) =<< (o .:? "field-looks")
  tcs <- maybe (pure NoTextCounts) (fmap TextCounts . mapM toUniqueTextCount) =<< (o .:? "text-counts")
  nss <- maybe (pure NoNumericFieldSummary) toNumericFieldSummary =<< (o .:? "numeric-summaries")
  pure $ RowCountSummary br tr nfs fas tcs nss
toRowCountSummary x = typeMismatch "Warden.Data.Marker.RowCountSummary" x
