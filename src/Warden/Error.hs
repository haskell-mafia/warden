{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Error (
    WardenError(..)
  , LoadError(..)
  , TraversalError(..)
  , MarkerError(..)
  , renderWardenError
) where

import           P

import           Data.Text (Text)
import qualified Data.Text as T

import           System.IO (FilePath)

import           Warden.Data.View

data WardenError =
    WardenLoadError LoadError
  | WardenNotImplementedError
  | WardenTraversalError TraversalError
  | WardenMarkerError MarkerError
  deriving (Eq, Show)

renderWardenError :: WardenError
                  -> Text
renderWardenError = ("warden: " <>) . render'
  where
    render' (WardenLoadError le) = renderLoadError le
    render' WardenNotImplementedError = "implement me!"
    render' (WardenTraversalError te) = renderTraversalError te
    render' (WardenMarkerError me) = renderMarkerError me

data LoadError =
    RowDecodeFailed ViewFile Text
  deriving (Eq, Show)

renderLoadError :: LoadError -> Text
renderLoadError = ("error loading view: " <>) . render'
  where
    render' (RowDecodeFailed vf e) = T.concat [
        "failed to decode row data in "
      , renderViewFile vf
      , " : "
      , e
      ]

data TraversalError =
    MaxDepthExceeded
  | EmptyView
  | NonViewFiles [NonViewFile]
  deriving (Eq, Show)

renderTraversalError :: TraversalError -> Text
renderTraversalError = ("traversal error: " <>) . render'
  where
    render' MaxDepthExceeded = "maximum traversal depth exceeded"
    render' EmptyView = "no files found in view"
    render' (NonViewFiles fs) =
         "extra files which don't seem to be part of a view: "
      <> (T.intercalate ", " $ renderNonViewFile <$> fs)

data MarkerError =
    MarkerDecodeError FilePath Text
  | ViewMarkerExistsError View FilePath
  | MarkerFileMismatchError ViewFile ViewFile
  | MarkerViewMismatchError View View
  | FileMarkerVersionError ViewFile
  | ViewMarkerVersionError View
  deriving (Eq, Show)

renderMarkerError :: MarkerError -> Text
renderMarkerError = ("marker error: " <>) . render'
  where
    render' (MarkerDecodeError fp t) =
      "failed to decode marker at " <> T.pack fp <> ": " <> t
    render' (ViewMarkerExistsError v mf) = T.concat [
        "marker already exists for view "
      , renderView v
      , " - remove "
      , T.pack mf
      , " or run with -f if you'd like to run the view checks again"
      ]
    render' (MarkerFileMismatchError a b) = T.concat [
        "cannot combine markers for files "
      , renderViewFile a
      , " and "
      , renderViewFile b
      ]
    render' (MarkerViewMismatchError a b) = T.concat [
        "cannot combine markers for views "
      , renderView a
      , " and "
      , renderView b
      ]
    render' (FileMarkerVersionError vf) = T.concat [
        "incompatible versions when combining markers for view file "
      , renderViewFile vf
      ]
    render' (ViewMarkerVersionError v) = T.concat [
        "incompatible versions when combining markers for view "
      , renderView v
      ]
