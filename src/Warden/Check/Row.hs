{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Warden.Check.Row (
    runRowCheck
  ) where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Foldl (Fold(..), FoldM(..), generalize)
import           Control.Monad.IO.Class (liftIO)
import           Control.Lens ((^.))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.Conduit (Consumer, ($$))
import qualified Data.Conduit.List as CL
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           System.IO (IO)

import           Warden.Chunk
import           Warden.Data
import           Warden.Debug
import           Warden.Error
import           Warden.Marker
import           Warden.Row

import           X.Control.Monad.Trans.Either (EitherT, left)

sinkFoldM :: Monad m => FoldM m a b -> Consumer a m b
sinkFoldM (FoldM f init extract) =
  lift init >>= CL.foldM f >>= lift . extract

-- FIXME: use schema
runRowCheck :: NumCPUs
            -> CheckParams
            -> Maybe Schema
            -> View
            -> NonEmpty ViewFile
            -> EitherT WardenError (ResourceT IO) CheckResult
runRowCheck caps ps@(CheckParams _s _sf _lb verb fce) _sch v vfs = do
  -- There should only be one view check, so exit early if we've already done
  -- it.
  existsP <- liftIO $ viewMarkerExists v
  when (existsP && noForce) $ do
    -- Fail with a more informative error if it's invalid.
    void $ readViewMarker v
    left . WardenMarkerError . ViewMarkerExistsError v $ viewToMarker v
  liftIO . debugPrintLn verb $ T.concat [
      "Running row checks on "
    , renderView v
    , "."
    ]
  (r, md) <- parseCheck caps ps vfs
  now <- liftIO utcNow
  writeViewMarker $ mkViewMarker v ViewRowCounts now md r
  pure $ RowCheckResult ViewRowCounts r
  where
    noForce = case fce of
      Force   -> False
      NoForce -> True

parseCheck :: NumCPUs -> CheckParams -> NonEmpty ViewFile -> EitherT WardenError (ResourceT IO) (CheckStatus, ViewMetadata)
parseCheck caps ps@(CheckParams s _sf lb verb _fce) vfs =
  fmap (finalizeSVParseState ps . resolveSVParseState . join) $
    mapM (parseViewFile caps verb s lb) (NE.toList vfs)

parseViewFile :: NumCPUs -> Verbosity -> Separator -> LineBound -> ViewFile -> EitherT WardenError (ResourceT IO) [SVParseState]
parseViewFile caps verb s lb vf = do
  liftIO . debugPrintLn verb $ T.concat [
      "Parsing view file "
    , renderViewFile vf
    , "."
    ]
  cs <- liftIO . chunk (chunksForCPUs caps) $ unViewFile vf
  mapConcurrently (\c -> readViewChunk s lb vf c $$ sinkFoldM (generalize parseViewFile')) $ NE.toList cs

parseViewFile' :: Fold Row SVParseState
parseViewFile' = Fold updateSVParseState initialSVParseState id

finalizeSVParseState :: CheckParams -> SVParseState -> (CheckStatus, ViewMetadata)
finalizeSVParseState ps sv = let st = resolveCheckStatus . NE.fromList $ [
                                      checkNumFields (sv ^. numFields)
                                    , checkTotalRows (sv ^. totalRows)
                                    , checkBadRows (sv ^. badRows)
                                    ] in
  (st, ViewMetadata sv ps)

checkNumFields :: Set FieldCount -> CheckStatus
checkNumFields s = case S.size s of
  0 -> CheckFailed $ NE.fromList [RowCheckFailure ZeroRows]
  1 -> CheckPassed
  _ -> CheckFailed $ NE.fromList [RowCheckFailure $ FieldCountMismatch s]

checkTotalRows :: RowCount -> CheckStatus
checkTotalRows (RowCount n)
  | n <= 0 = CheckFailed $ NE.fromList [RowCheckFailure ZeroRows]
  | otherwise = CheckPassed

checkBadRows :: RowCount -> CheckStatus
checkBadRows (RowCount 0) = CheckPassed
checkBadRows n = CheckFailed $ NE.fromList [RowCheckFailure $ HasBadRows n]
