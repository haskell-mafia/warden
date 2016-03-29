{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
    check
  , fileCheck
  , infer
  , sanity
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

sanity :: WardenParams
       -> View
       -> SanityParams
       -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
sanity _wps _view _sps = left WardenNotImplementedError

checkViewFiles :: WardenParams
               -> CheckParams
               -> View
               -> NonEmpty ViewFile
               -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
checkViewFiles wps ps v vfs = do
  schema <- maybe (pure Nothing) (fmap Just . readSchema) $ checkSchemaFile ps
  frs <- fmap join $ traverse (forM File.fileChecks) $ (File.runFileCheck wps (checkVerbosity ps) (checkForce ps)) <$> vfs
  rr <- Row.runRowCheck wps ps schema v vfs
  pure $ rr <| frs

infer :: Verbosity
      -> FieldMatchRatio
      -> [FilePath]
      -> EitherT WardenError (ResourceT IO) Schema
infer v fmr fps = case nonEmpty fps of
  Nothing -> left $ WardenInferenceError NoViewMarkersError
  Just fps' -> do
    vms <- mapM readViewMarker fps'
    vms' <- withErr $ validateViewMarkers vms
    cs <- withErr $ countCompatibleFields vms'
    liftIO . debugPrintLn v $ renderFieldHistogramVector cs
    tcs <- withErr $ inferForms vms'
    withErr $ generateSchema fmr tcs (totalViewRows vms') cs
  where
    renderFieldHistogramVector hs =
      T.intercalate "\n" . V.toList .
        V.map (\(i, t) -> (T.pack $ show i) <> ": " <> t) .
          V.indexed $ V.map renderFieldHistogram hs

    withErr = firstEitherT WardenInferenceError . hoistEither
