{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Data.Marker (
    CheckResultSummary(..)
  , CheckResultType(..)
  , FileMarker(..)
  , MarkerFailure(..)
  , MarkerStatus(..)
  , MarkerVersion(..)
  , ViewMarker(..)
  , ViewMetadata(..)
  , currentMarkerVersion
  , filePathChar
  , fileToMarker
  , markerToFile
  , markerToView
  , mkViewMarker
  , viewToMarker
  ) where

import           Data.Attoparsec.Text (IResult(..), Parser, parse)
import           Data.Attoparsec.Text (string, satisfy, manyTill)
import           Data.Char (ord)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Data.Text as T

import           Delorean.Local.DateTime (DateTime)

import           System.FilePath ((</>), takeFileName, replaceFileName)
import           System.FilePath (takeDirectory)
import           System.IO (FilePath)

import           P

import           Warden.Data.Check
import           Warden.Data.Row
import           Warden.Data.View

data MarkerVersion =
    V1
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
  , fmViewFile :: !ViewFile
  , fmTimestamp :: !DateTime
  , fmCheckResults :: ![CheckResultSummary]
  } deriving (Eq, Show)

markerSuffix :: FilePath
markerSuffix = ".warden"

fileToMarker :: ViewFile -> FilePath
fileToMarker (ViewFile vf) =
  let fileName   = takeFileName vf
      markerFile = "_" <> fileName <> markerSuffix in
  replaceFileName vf markerFile

markerToFile :: View -> FilePath -> Maybe ViewFile
markerToFile v fp
  | not (isViewFile v fp) = Nothing
  | otherwise             = do
      fn <- finalize . parse fileMarker . T.pack $ takeFileName fp
      pure . ViewFile $ replaceFileName fp fn
  where
    fileMarker :: Parser FilePath
    fileMarker =
      string "_" *> manyTill filePathChar (string (T.pack markerSuffix))

    finalize (Partial c)  = finalize $ c ""
    finalize (Done "" "") = Nothing
    finalize (Done "" r)  = Just r
    finalize _            = Nothing

viewToMarker :: View -> FilePath
viewToMarker (View v) =
  v </> ("_view" <> markerSuffix)

markerToView :: FilePath -> Maybe View
markerToView fp =
  let v = takeDirectory fp
      fn = takeFileName fp in
  case fn of
    "_view.warden" -> Just $ View v
    _              -> Nothing

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
  , vmView :: !View
  , vmTimestamp :: !DateTime
  , vmCheckResults :: ![CheckResultSummary]
  , vmMetadata :: !ViewMetadata
  } deriving (Eq, Show)

mkViewMarker :: View -> CheckDescription -> DateTime -> ViewMetadata -> CheckStatus -> ViewMarker
mkViewMarker v dsc dt vm cs =
  let crs = [summarizeResult RowResult dsc cs] in
  ViewMarker currentMarkerVersion v dt crs vm

data ViewMetadata =
  ViewMetadata {
    viewCounts :: !SVParseState
  } deriving (Eq, Show)
