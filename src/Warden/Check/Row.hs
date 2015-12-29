{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Check.Row (
    rowCountsCheck
  , rowParseC
  , runRowCheck
  ) where

import           Control.Foldl (Fold(..), generalize, impurely)
import           Control.Lens ((^.))

import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)

import           P
import qualified Pipes.Prelude as PP

import           System.IO (IO)

import           Warden.Data
import           Warden.Error
import           Warden.Rows

import           X.Control.Monad.Trans.Either (EitherT)

runRowCheck :: Separator -> NonEmpty ViewFile -> RowCheck -> EitherT WardenError IO CheckResult
runRowCheck s vfs (RowCheck desc chk) = do
  r <- impurely PP.foldM chk (readSVView s vfs)
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

checkTotalRows :: Integer -> CheckStatus
checkTotalRows n
  | n <= 0 = CheckFailed $ NE.fromList [RowCheckFailure ZeroRows]
  | otherwise = CheckPassed

checkBadRows :: Integer -> CheckStatus
checkBadRows 0 = CheckPassed
checkBadRows n = CheckFailed $ NE.fromList [RowCheckFailure $ HasBadRows n]
