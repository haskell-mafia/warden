{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Error (
    WardenError(..)
  , renderWardenError
) where

import           P

import           Data.Text (Text)

data WardenError = LoadError Text
  deriving (Eq, Show)

renderWardenError :: WardenError
                  -> Text
renderWardenError = ("warden: " <>) . render'
  where
    render' (LoadError e) = "error loading dataset: " <> e
