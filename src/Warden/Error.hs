{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Error (
  WardenError(..)
) where

import Data.Text

data WardenError = FSError Text
