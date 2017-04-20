{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands(
    check
  , extractNumericFields
  , failedMarkers
  , fileCheck
  , infer
  , sanity
  , summarizeMarkers
  , validateSchema
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
  let idf = checkIncludeDotFiles ps in
  traverseView idf v >>= (checkViewFiles wps ps v)

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
sanity wps view sps =
  let idf = sanityIncludeDotFiles sps in do
  vfs <- traverseView idf view
  frs <- fmap join . traverse (forM File.fileChecks) $ (File.runFileCheck wps (sanityVerbosity sps) (sanityForce sps)) <$> vfs
  pure frs

checkViewFiles :: WardenParams
               -> CheckParams
               -> View
               -> NonEmpty ViewFile
               -> EitherT WardenError (ResourceT IO) (NonEmpty CheckResult)
checkViewFiles wps ps v vfs = do
  schema <- maybe (pure Nothing) (fmap Just . readSchema) $ checkSchemaFile ps
  frs <- fmap join . traverse (forM File.fileChecks) $
    (File.runFileCheck wps (checkVerbosity ps) (checkForce ps)) <$> vfs
  rr <- Row.runRowCheck wps ps schema v vfs
  pure $ rr <| frs

infer :: Verbosity
      -> FieldMatchRatio
      -> InferUsingFailedChecks
      -> [FilePath]
      -> EitherT WardenError (ResourceT IO) Schema
infer v fmr fc fps = case nonEmpty fps of
  Nothing -> left $ WardenInferenceError NoViewMarkersError
  Just fps' -> do
    vms <- mapM readViewMarker fps'
    vms' <- withErr $ validateViewMarkers fc vms
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

validateSchema :: SchemaFile
               -> EitherT WardenError (ResourceT IO) ()
validateSchema sf =
  void $ readSchema sf

summarizeMarkers :: [FilePath]
                 -> EitherT WardenError (ResourceT IO) ()
summarizeMarkers _fs =
  left WardenNotImplementedError

failedMarkers :: [FilePath]
              -> EitherT WardenError (ResourceT IO) [FilePath]
failedMarkers fs =
  filterM markerFailed fs
  where
    markerFailed mf = do
      vm <- readViewMarker mf
      pure $ or (fmap resultSummaryFailed $ vmCheckResults vm)

extractNumericFields
  :: FilePath
  -> [FilePath]
  -> EitherT WardenError (ResourceT IO) ()
extractNumericFields _outp _fs =
  left WardenNotImplementedError

