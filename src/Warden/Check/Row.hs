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

import           P

import           System.IO (IO)

import           Warden.Data
import           Warden.Error
import           Warden.Marker
import           Warden.Row

import           X.Control.Monad.Trans.Either (EitherT)

sinkFoldM :: Monad m => FoldM m a b -> Consumer a m b
sinkFoldM (FoldM f init extract) =
  lift init >>= CL.foldM f >>= lift . extract

runRowCheck :: Separator -> View -> LineBound -> NonEmpty ViewFile -> EitherT WardenError (ResourceT IO) CheckResult
runRowCheck s v lb vfs =
  let desc = CheckDescription "row parsing/field counts" in do
  (r, md) <- parseCheck s lb vfs
  now <- liftIO utcNow
  writeViewMarker $ mkViewMarker v desc now md r
  pure $ RowCheckResult desc r

parseCheck :: Separator -> LineBound -> NonEmpty ViewFile -> EitherT WardenError (ResourceT IO) (CheckStatus, ViewMetadata)
parseCheck s lb vfs =
  fmap (finalizeSVParseState . resolveSVParseState . NE.toList) $
    mapConcurrently (parseViewFile s lb) vfs

parseViewFile :: Separator -> LineBound -> ViewFile -> EitherT WardenError (ResourceT IO) SVParseState
parseViewFile s lb vf = do
  readViewFile s lb vf $$ sinkFoldM (generalize parseViewFile')

parseViewFile' :: Fold Row SVParseState
parseViewFile' = Fold updateSVParseState initialSVParseState id

finalizeSVParseState :: SVParseState -> (CheckStatus, ViewMetadata)
finalizeSVParseState sv = let st = resolveCheckStatus . NE.fromList $ [
                                  checkNumFields (sv ^. numFields)
                                , checkTotalRows (sv ^. totalRows)
                                , checkBadRows (sv ^. badRows)
                                ] in
  (st, ViewMetadata sv)

checkNumFields :: [FieldCount] -> CheckStatus
checkNumFields [] = CheckFailed $ NE.fromList [RowCheckFailure ZeroRows]
checkNumFields [_] = CheckPassed
checkNumFields xs  = CheckFailed $ NE.fromList [RowCheckFailure $ FieldCountMismatch xs]

checkTotalRows :: RowCount -> CheckStatus
checkTotalRows (RowCount n)
  | n <= 0 = CheckFailed $ NE.fromList [RowCheckFailure ZeroRows]
  | otherwise = CheckPassed

checkBadRows :: RowCount -> CheckStatus
checkBadRows (RowCount 0) = CheckPassed
checkBadRows n = CheckFailed $ NE.fromList [RowCheckFailure $ HasBadRows n]
