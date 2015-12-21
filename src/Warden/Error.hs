{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Error (
    WardenError(..)
  , TraversalError(..)
  , renderWardenError
) where

import           P

import           Data.Text (Text)
import qualified Data.Text as T

import           Warden.Data.View

data WardenError =
    LoadError Text
  | WardenNotImplementedError
  | WardenTraversalError TraversalError
  deriving (Eq, Show)

data TraversalError =
    MaxDepthExceeded
  | EmptyView
  | NonViewFiles [NonViewFile]
  deriving (Eq, Show)

renderWardenError :: WardenError
                  -> Text
renderWardenError = ("warden: " <>) . render'
  where
    render' (LoadError e) = "error loading dataset: " <> e
    render' WardenNotImplementedError = "implement me!"
    render' (WardenTraversalError te) = renderTraversalError te

renderTraversalError :: TraversalError -> Text
renderTraversalError = ("traversal error: " <>) . render'
  where
    render' MaxDepthExceeded = "maximum traversal depth exceeded"
    render' EmptyView = "no files found in view"
    render' (NonViewFiles fs) =
         "extra files which don't seem to be part of a view: "
      <> (T.intercalate ", " $ renderNonViewFile <$> fs)
