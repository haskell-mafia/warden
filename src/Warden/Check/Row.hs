{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Warden.Check.Row (
    runRowCheck
  , rowParseC
  ) where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Foldl (Fold(..), FoldM(..), generalize)
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
import           Warden.Row

import           X.Control.Monad.Trans.Either (EitherT)

sinkFoldM :: Monad m => FoldM m a b -> Consumer a m b
sinkFoldM (FoldM f init extract) =
  lift init >>= CL.foldM f >>= lift . extract

runRowCheck :: Separator -> LineBound -> NonEmpty ViewFile -> RowCheck -> EitherT WardenError (ResourceT IO) CheckResult
runRowCheck s lb vfs (RowCheck d chk) = do
  r <- chk s lb vfs
  pure $ RowCheckResult d r

rowParseC :: RowCheck
rowParseC =
  RowCheck (CheckDescription "xSV field counts") parseCheck
           

parseCheck :: Separator -> LineBound -> NonEmpty ViewFile -> EitherT WardenError (ResourceT IO) CheckStatus
parseCheck s lb vfs =
  fmap (finalizeSVParseState . resolveSVParseState . NE.toList) $
    mapConcurrently (parseViewFile s lb) vfs

parseViewFile :: Separator -> LineBound -> ViewFile -> EitherT WardenError (ResourceT IO) SVParseState
parseViewFile s lb vf = do
  readViewFile s lb vf $$ sinkFoldM (generalize parseViewFile')

parseViewFile' :: Fold Row SVParseState
parseViewFile' = Fold updateSVParseState initialSVParseState id

finalizeSVParseState :: SVParseState -> CheckStatus
finalizeSVParseState sv = resolveCheckStatus . NE.fromList $ [
    checkNumFields (sv ^. numFields)
  , checkTotalRows (sv ^. totalRows)
  , checkBadRows (sv ^. badRows)
  ]

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
