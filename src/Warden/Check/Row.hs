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

import           Delorean.Local.Date (Date)

import           P

import           System.IO (IO)

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
runRowCheck wps ps@(CheckParams _s _sf _lb verb _fce) sch v vfs = do
  liftIO . debugPrintLn verb $ T.concat [
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
parseCheck caps ps@(CheckParams s _sf lb verb _fce) sch vfs =
  let dates = S.fromList . NE.toList $ vfDate <$> vfs in
  fmap (finalizeSVParseState ps sch dates vfs . resolveSVParseState . join) $
    mapM (parseViewFile caps verb s lb) (NE.toList vfs)

parseViewFile :: NumCPUs
              -> Verbosity
              -> Separator
              -> LineBound
              -> ViewFile
              -> EitherT WardenError (ResourceT IO) [SVParseState]
parseViewFile caps verb s lb vf = do
  liftIO . debugPrintLn verb $ T.concat [
      "Parsing view file "
    , renderViewFile vf
    , "."
    ]
  cs <- liftIO . chunk (chunksForCPUs caps) $ viewFilePath vf
  mapConcurrently (\c -> readViewChunk s lb vf c $$ sinkFoldM (generalize parseViewFile')) $ NE.toList cs

parseViewFile' :: Fold Row SVParseState
parseViewFile' = Fold updateSVParseState initialSVParseState id

finalizeSVParseState :: CheckParams
                     -> Maybe Schema
                     -> Set Date
                     -> NonEmpty ViewFile
                     -> SVParseState
                     -> (CheckStatus, ViewMetadata)
finalizeSVParseState ps sch ds vfs sv =
  let st = resolveCheckStatus . NE.fromList $ [
               checkNumFields sch (sv ^. numFields)
             , checkTotalRows (sv ^. totalRows)
             , checkBadRows (sv ^. badRows)
             ]
      vfs' = S.fromList $ NE.toList vfs in
  (st, ViewMetadata sv ps ds vfs')

checkNumFields :: Maybe Schema -> Set FieldCount -> CheckStatus
checkNumFields sch s = case S.size s of
  0 -> CheckFailed $ NE.fromList [RowCheckFailure ZeroRows]
  1 -> maybe CheckPassed (validateSchemaFields s) sch
  _ -> CheckFailed $ NE.fromList [RowCheckFailure $ FieldCountMismatch s]

validateSchemaFields :: Set FieldCount -> Schema -> CheckStatus
validateSchemaFields s (Schema _v cnt _fs) =
  let s' = S.singleton cnt
      d = S.difference s s' in
  case S.toList d of
    [] -> CheckPassed
    _ -> CheckFailed $
            NE.fromList [SchemaCheckFailure $ IncorrectFieldCount cnt d]

checkTotalRows :: RowCount -> CheckStatus
checkTotalRows (RowCount n)
  | n <= 0 = CheckFailed $ NE.fromList [RowCheckFailure ZeroRows]
  | otherwise = CheckPassed

checkBadRows :: RowCount -> CheckStatus
checkBadRows (RowCount 0) = CheckPassed
checkBadRows n = CheckFailed $ NE.fromList [RowCheckFailure $ HasBadRows n]
