{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Warden.Data (
    WardenCheck(..)
  , WardenStatus(..)
  , CheckResult(..)
  , RowSchema(..)

  , module Warden.Data.Numeric
  , module Warden.Data.SeparatedValues
  ) where

import           Data.Text (Text)

import           P

import           Warden.Data.Numeric
import           Warden.Data.SeparatedValues

data WardenStatus = Green
                    -- ^ No issues detected.
                  | Yellow
                    -- ^ Some values are concerning and should be investigated
                    --   by a human.
                  | Red
                    -- ^ At least one check failed, processing should not
                    --   proceed without human intervention.
                  | Unknown
                    -- ^ We broke, or weren't given enough data.
  deriving (Eq, Show, Ord)

-- FIXME(sharif): this could use more structure once we have a better idea of
--                what failures look like - row(s) affected, et cetera
data CheckResult = CheckResult WardenStatus Text

data RowSchema a = RowSchema
  { fromRow :: Row -> Maybe a }

data WardenCheck a b = WardenCheck
  { initial  :: a
  , update   :: a -> RowSchema b -> a
  , finalize :: a -> CheckResult
  }
