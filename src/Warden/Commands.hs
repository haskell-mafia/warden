{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
    check
  , fileCheck
  , infer
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import qualified Data.Text as T
import qualified Data.Vector as V

import           P

import           System.Directory (makeAbsolute)
import           System.FilePath (takeDirectory)
import           System.IO (IO, FilePath)

import qualified Warden.Check.File as File
import qualified Warden.Check.Row as Row

import           Warden.Data
import           Warden.Debug
import           Warden.Error
import           Warden.Inference
import           Warden.Marker
import           Warden.Schema
import           Warden.View

import           X.Control.Monad.Trans.Either (EitherT, left, hoistEither)
import           X.Control.Monad.Trans.Either (firstEitherT)

check :: WardenParams
      -> View
      -> CheckParams
      -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
check wps v ps =
  traverseView v >>= (checkViewFiles wps ps v)

fileCheck :: WardenParams
          -> ViewFile
          -> CheckParams
          -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
fileCheck wps vf ps = do
  vf' <- liftIO . makeAbsolute $ viewFilePath vf
  let view = View $ takeDirectory vf'
  checkViewFiles wps ps view $ vf :| []

checkViewFiles :: WardenParams
               -> CheckParams
               -> View
               -> NonEmpty ViewFile
               -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
checkViewFiles wps ps@(CheckParams _s sf _lb verb fce) v vfs = do
  schema <- maybe (pure Nothing) (fmap Just . readSchema) sf
  frs <- fmap join $ traverse (forM File.fileChecks) $ (File.runFileCheck wps verb fce) <$> vfs
  rr <- Row.runRowCheck wps ps schema v vfs
  pure $ rr <| frs

infer :: Verbosity
      -> [FilePath]
      -> EitherT WardenError (ResourceT IO) Schema
infer v fps = case nonEmpty fps of
  Nothing -> left $ WardenInferenceError NoViewMarkersError
  Just fps' -> do
    vms <- mapM readViewMarker fps'
    cs <- firstEitherT WardenInferenceError . hoistEither $
            countCompatibleFields vms
    liftIO . debugPrintLn v $ renderFieldHistogramVector cs
    firstEitherT WardenInferenceError . hoistEither $ generateSchema cs
  where
    renderFieldHistogramVector hs =
      T.intercalate "\n" . V.toList .
        V.map (\(i, t) -> (T.pack $ show i) <> ": " <> t) .
          V.indexed $ V.map renderFieldHistogram hs
