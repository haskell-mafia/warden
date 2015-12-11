{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data (
    FileCheck
  , CheckStatus(..)
  , Failure(..)
  , Insanity(..)
  , renderFailure

  , module X
  ) where

import           Data.Text (Text)

import           P

import           System.IO

import           X.Control.Monad.Trans.Either (EitherT)

import           Warden.Error
import           Warden.Data.Numeric as X
import           Warden.Data.SeparatedValues as X

data CheckStatus = CheckPassed | CheckFailed
  deriving (Eq, Show, Ord, Enum)

data Failure =
    SanityCheckFailure Insanity
  deriving (Eq, Show)

data Insanity =
    EmptyFile
  | IrregularFile
  deriving (Eq, Show)

renderFailure :: Failure -> Text
renderFailure (SanityCheckFailure f) =
  "sanity checks failed: " <> renderInsanity f

renderInsanity :: Insanity -> Text
renderInsanity EmptyFile = "file of zero size"
renderInsanity IrregularFile = "not a regular file"

type FileCheck = (FilePath -> EitherT WardenError IO [CheckResult])
