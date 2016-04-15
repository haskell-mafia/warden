{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Warden.Check.Row (
    runRowCheck
  ) where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Foldl (FoldM(..))
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.Conduit (Consumer, ($$))
import qualified Data.Conduit.List as CL
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import           Delorean.Local.Date (Date)

import           P

import           System.IO (IO)
import           System.Random.MWC (Gen, createSystemRandom)

import           Warden.Chunk
import           Warden.Data
import           Warden.Debug
import           Warden.Error
import           Warden.Marker
import           Warden.Row

import           X.Control.Monad.Trans.Either (EitherT)

sinkFoldM :: Monad m => FoldM m a b -> Consumer a m b
sinkFoldM (FoldM f init extract) =
  lift init >>= CL.foldM f >>= lift . extract

runRowCheck :: WardenParams
            -> CheckParams
            -> Maybe Schema
            -> View
            -> NonEmpty ViewFile
            -> EitherT WardenError (ResourceT IO) CheckResult
runRowCheck wps ps sch v vfs = do
  liftIO . debugPrintLn (checkVerbosity ps) $ T.concat [
      "Running row checks on "
    , renderView v
    , "."
    ]
  (r, md) <- parseCheck (wpCaps wps) ps sch vfs
  now <- liftIO utcNow
  writeViewMarker $ mkViewMarker wps v ViewRowCounts now md r
  pure $ RowCheckResult ViewRowCounts r

parseCheck :: NumCPUs
           -> CheckParams
           -> Maybe Schema
           -> NonEmpty ViewFile
           -> EitherT WardenError (ResourceT IO) (CheckStatus, ViewMetadata)
parseCheck caps ps sch vfs =
  let dates = S.fromList . NE.toList $ vfDate <$> vfs
      fft = checkFreeformThreshold ps
      s = checkSeparator ps
      verb = checkVerbosity ps
      lb = checkLineBound ps
      st = checkSamplingType ps in do
  g <- liftIO createSystemRandom
  ss <- mapM (parseViewFile caps verb s lb fft st) (NE.toList vfs)
  finalState <- liftIO $ resolveSVParseState fft g st ss
  liftIO $ finalizeSVParseState ps sch dates vfs finalState

parseViewFile :: NumCPUs
              -> Verbosity
              -> Separator
              -> LineBound
              -> TextFreeformThreshold
              -> SamplingType
              -> ViewFile
              -> EitherT WardenError (ResourceT IO) SVParseState
parseViewFile caps verb s lb fft st vf = do
  cs <- liftIO . chunk (chunksForCPUs caps) $ viewFilePath vf
  liftIO . debugPrintLn verb $ T.concat [
      "Parsing view file "
    , renderViewFile vf
    , " in "
    , renderIntegral (NE.length cs)
    , " chunks."
    ]
  ss <- mapConcurrently (\c -> readViewChunk s lb vf c $$ sinkParse) $ NE.toList cs
  g <- liftIO createSystemRandom
  liftIO $ resolveSVParseState fft g st ss
  where
    -- FIXME: could probably get away with fewer Gens created
    sinkParse = do
      g <- liftIO createSystemRandom
      sinkFoldM (parseViewFile' fft g st)

parseViewFile' :: TextFreeformThreshold
               -> Gen (PrimState IO)
               -> SamplingType
               -> FoldM (EitherT WardenError (ResourceT IO)) Row SVParseState
parseViewFile' fft g st = FoldM update' (pure initialSVParseState) pure
  where
    update' x y = updateSVParseState fft g st x y

finalizeSVParseState :: CheckParams
                     -> Maybe Schema
                     -> Set Date
                     -> NonEmpty ViewFile
                     -> SVParseState
                     -> IO (CheckStatus, ViewMetadata)
finalizeSVParseState ps sch ds vfs sv =
  let st = resolveCheckStatus . NE.fromList $ [
               checkFieldAnomalies sch (sv ^. fieldLooks)
             , checkFormAnomalies sch (sv ^. textCounts)
             , checkFieldCounts (sv ^. numFields)
             , checkTotalRows (sv ^. totalRows)
             , checkBadRows (sv ^. badRows)
             ]
      vfs' = S.fromList $ NE.toList vfs in do
  rcs <- summarizeSVParseState sv
  pure (st, ViewMetadata rcs ps ds vfs')

checkTotalRows :: RowCount -> CheckStatus
checkTotalRows (RowCount n)
  | n <= 0 = CheckFailed . pure $ RowCheckFailure ZeroRows
  | otherwise = CheckPassed

checkBadRows :: RowCount -> CheckStatus
checkBadRows (RowCount 0) = CheckPassed
checkBadRows n = CheckFailed . pure . RowCheckFailure $ HasBadRows n

-- | Zero field counts -> we got zero rows -> fail.
--   Multiple field counts -> at least some rows are borked -> fail.
--   One field count -> pass.
checkFieldCounts :: Set FieldCount -> CheckStatus
checkFieldCounts fcs =
  case S.size fcs of
    0 -> CheckFailed . pure $ RowCheckFailure ZeroRows
    1 -> CheckPassed
    _ -> CheckFailed . pure . RowCheckFailure $ FieldCountMismatch fcs

-- FIXME: model check dependencies better, e.g., the field types shouldn't
-- be compared if the field counts don't match.
checkFieldAnomalies :: Maybe Schema -> FieldLookCount -> CheckStatus
checkFieldAnomalies Nothing _ = CheckPassed
checkFieldAnomalies _ NoFieldLookCount = CheckPassed
checkFieldAnomalies (Just (Schema SchemaV1 fs)) (FieldLookCount as) =
  let schemaCount = FieldCount  $ V.length fs
      obsCount = FieldCount $ V.length as in
  if schemaCount /= obsCount
    then
      CheckFailed . pure . SchemaCheckFailure $ FieldCountObservationMismatch schemaCount obsCount
    else
      let rs = V.zipWith (\t' (idx',oc') -> fieldAnomalies t' oc' (FieldIndex idx')) (schemaFieldType `V.map` fs) (V.indexed as)
          anoms = catMaybes $ V.toList rs in
      case nonEmpty anoms of
        Nothing ->
          CheckPassed
        Just anoms' ->
          CheckFailed $ (SchemaCheckFailure . FieldAnomalyFailure) <$> anoms'

checkFormAnomalies :: Maybe Schema -> TextCounts -> CheckStatus
checkFormAnomalies Nothing _ = CheckPassed
checkFormAnomalies _ NoTextCounts = CheckPassed
checkFormAnomalies (Just (Schema SchemaV1 fs)) (TextCounts cs) =
  let rs = V.zipWith (\f' (idx',tc') -> formAnomalies f' tc' (FieldIndex idx')) (schemaFieldForm `V.map` fs) (V.indexed cs)
      anoms = catMaybes $ V.toList rs in
  case nonEmpty anoms of
    Nothing ->
      CheckPassed
    Just anoms' ->
      CheckFailed $ (SchemaCheckFailure . FieldAnomalyFailure) <$> anoms'
