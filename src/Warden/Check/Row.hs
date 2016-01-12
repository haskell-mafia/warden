{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Warden.Check.Row (
    rowCountsCheck
  , rowParseC
  , runRowCheck
  ) where

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
runRowCheck s lb vfs (RowCheck desc chk) = do
  r <- readView s lb vfs $$ sinkFoldM chk 
  pure $ RowCheckResult desc r

rowCountsCheck :: RowCheck
rowCountsCheck = 
  RowCheck (CheckDescription "xSV field counts") (generalize rowParseC)

-- FIXME: do something with final state
rowParseC :: Fold Row CheckStatus
rowParseC = finalize <$> (Fold updateSVParseState initialSVParseState id)
  where
    -- FIXME: field counts
    finalize sv = resolveCheckStatus . NE.fromList $ [
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
