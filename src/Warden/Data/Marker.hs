{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.Marker (
    CheckResultSummary(..)
  , CheckResultType(..)
  , DateRange(..)
  , FileMarker(..)
  , MarkerFailure(..)
  , MarkerStatus(..)
  , MarkerVersion(..)
  , RowCountSummary(..)
  , ViewMarker(..)
  , ViewMetadata(..)
  , currentMarkerVersion
  , dateRange
  , filePathChar
  , fileToMarker
  , markerToFile
  , mkFileMarker
  , mkViewMarker
  , summarizeSVParseState
  , viewMarkerPath
  ) where

import           Control.Lens ((^.))

import           Data.Attoparsec.Text (IResult(..), Parser, parse)
import           Data.Attoparsec.Text (string, satisfy, manyTill')
import           Data.Char (ord)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           Delorean.Local.Date (Date, renderDate)
import           Delorean.Local.DateTime (DateTime(..))

import           System.FilePath (takeFileName, replaceFileName, joinPath)
import           System.IO (FilePath)

import           P

import           Warden.Data.Check
import           Warden.Data.Param
import           Warden.Data.Row
import           Warden.Data.TextCounts
import           Warden.Data.View

data MarkerVersion =
    MarkerV1
  deriving (Eq, Show, Ord, Bounded, Enum)

currentMarkerVersion :: MarkerVersion
currentMarkerVersion = maxBound

data CheckResultType =
    FileResult
  | RowResult
  deriving (Eq, Show, Ord, Bounded, Enum)

newtype MarkerFailure =
  MarkerFailure {
    unMarkerFailure :: NonEmpty Text
  } deriving (Eq, Show)

data MarkerStatus =
    MarkerPass
  | MarkerFail !MarkerFailure
  deriving (Eq, Show)

data CheckResultSummary =
  CheckResultSummary {
      summaryStatus :: !MarkerStatus
    , summaryDescription :: !CheckDescription
    , summaryResultType :: !CheckResultType
  } deriving (Eq, Show)

data DateRange =
    NoDates
  | DateRange Date Date
  deriving (Eq, Show)

dateRangePartition :: DateRange -> Text
dateRangePartition (DateRange start end) = T.concat [
    renderDate start
  , "_"
  , renderDate end
  ]
dateRangePartition NoDates = "no-dates"

dateRange :: Set Date -> DateRange
dateRange ds
  | S.null ds = NoDates
  | otherwise = DateRange (S.findMin ds) (S.findMax ds)

summarizeFailures :: NonEmpty Failure -> MarkerFailure
summarizeFailures fs = MarkerFailure $ renderFailure <$> fs

summarizeStatus :: CheckStatus -> MarkerStatus
summarizeStatus CheckPassed = MarkerPass
summarizeStatus (CheckFailed fs) = MarkerFail $ summarizeFailures fs

summarizeResult :: CheckResultType -> CheckDescription -> CheckStatus -> CheckResultSummary
summarizeResult typ dsc st =
  let sst = summarizeStatus st in
  CheckResultSummary sst dsc typ

data FileMarker =
  FileMarker {
    fmVersion :: !MarkerVersion
  , fmWardenParams :: !WardenParams
  , fmViewFile :: !ViewFile
  , fmTimestamp :: !DateTime
  , fmCheckResults :: ![CheckResultSummary]
  } deriving (Eq, Show)

mkFileMarker :: WardenParams
             -> ViewFile
             -> CheckDescription
             -> DateTime
             -> CheckStatus
             -> FileMarker
mkFileMarker wps v dsc dt cs =
  let crs = [summarizeResult FileResult dsc cs] in
  FileMarker currentMarkerVersion wps v dt crs

markerSuffix :: FilePath
markerSuffix = ".warden"

fileToMarker :: ViewFile -> FilePath
fileToMarker vf =
  let fileName   = takeFileName $ viewFilePath vf
      markerFile = "_" <> fileName <> markerSuffix in
  replaceFileName (viewFilePath vf) markerFile

markerToFile :: FilePath -> Maybe ViewFile
markerToFile fp = case viewFile fp of
  Left _ -> Nothing
  Right vf -> do
    fn <- finalize . parse fileMarker . T.pack $ takeFileName fp
    let fp' = FilePart . T.pack $ replaceFileName (T.unpack . unFilePart $ vfFilePart vf) fn
    pure $ vf { vfFilePart = fp' }
  where
    fileMarker :: Parser FilePath
    fileMarker =
      string "_" *> manyTill' filePathChar (string $ T.pack markerSuffix)

    finalize (Partial c)  = finalize $ c ""
    finalize (Done "" "") = Nothing
    finalize (Done "" r)  = Just r
    finalize _            = Nothing

viewMarkerPath :: ViewMarker -> FilePath
viewMarkerPath vm =
  let meta = vmMetadata vm
      wps = vmWardenParams vm
      view = unView $ vmView vm
      ts = T.unpack . renderDateTimeDate $ vmTimestamp vm
      dates = vmDates meta
      dr = T.unpack . dateRangePartition $ dateRange dates
      rid = T.unpack . renderRunId $ wpRunId wps in
  joinPath [
      "_warden"
    , "marker"
    , "view"
    , view
    , dr
    , ts
    , rid <> ".warden"
    ]
  where
    renderDateTimeDate (DateTime d _t) =
      renderDate d

filePathChar :: Parser Char
filePathChar = satisfy (not . bad)
  where
    bad c = or [
        -- This fails some filenames which POSIX might call valid; this is
        -- by design.
        (ord c) < 32
      , c == '/'
      ]

data ViewMarker =
  ViewMarker {
    vmVersion :: !MarkerVersion
  , vmWardenParams :: !WardenParams
  , vmView :: !View
  , vmTimestamp :: !DateTime
  , vmCheckResults :: ![CheckResultSummary]
  , vmMetadata :: !ViewMetadata
  } deriving (Eq, Show)

mkViewMarker :: WardenParams
             -> View
             -> CheckDescription
             -> DateTime
             -> ViewMetadata
             -> CheckStatus
             -> ViewMarker
mkViewMarker wps v dsc dt vm cs =
  let crs = [summarizeResult RowResult dsc cs] in
  ViewMarker currentMarkerVersion wps v dt crs vm

data RowCountSummary =
  RowCountSummary {
    rcsBadRows :: !RowCount
  , rcsTotalRows :: !RowCount
  , rcsNumFields :: !(Set FieldCount)
  , rcsFieldLooks :: !FieldLookCount
  , rcsTextCounts :: !TextCounts
  } deriving (Eq, Show)

summarizeSVParseState :: SVParseState -> RowCountSummary
summarizeSVParseState ps =
  RowCountSummary
    (ps ^. badRows)
    (ps ^. totalRows)
    (ps ^. numFields)
    (ps ^. fieldLooks)
    (ps ^. textCounts)

data ViewMetadata =
  ViewMetadata {
      vmViewCounts :: !RowCountSummary
    , vmCheckParams :: !CheckParams
    , vmDates :: !(Set Date)
    , vmViewFiles :: !(Set ViewFile)
  } deriving (Eq, Show)
