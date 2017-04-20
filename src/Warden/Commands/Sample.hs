{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands.Sample (
    extractNumericFields
  , identical
) where

import           Control.Monad.Trans.Resource (ResourceT)

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           P

import           System.IO (IO, FilePath)

import           Warden.Data.Marker
import           Warden.Data.Numeric
import           Warden.Data.Sampling
import           Warden.Error
import           Warden.Marker

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (left, hoistEither)

extractNumericFields
  :: FilePath
  -> [FilePath]
  -> EitherT WardenError (ResourceT IO) ()
extractNumericFields _outp fs =
  case nonEmpty fs of
    Nothing ->
      pure ()
    Just fs' -> do
      nss <- mapM readNumericSummary fs'
      _ss <- hoistEither . first WardenSampleError $ combineMarkerSamples nss
      left WardenNotImplementedError

readNumericSummary
  :: FilePath
  -> EitherT WardenError (ResourceT IO) (V.Vector NumericSummary)
readNumericSummary f =
  (readViewMarker f) >>= \m ->
    case rcsNumericSummaries (vmViewCounts $ vmMetadata m) of
      NoNumericFieldSummary ->
        left . WardenSampleError $ NoNumericSummaries f
      NumericFieldSummary ss ->
        pure ss

combineMarkerSamples
  :: NonEmpty (V.Vector NumericSummary)
  -> Either SampleError (V.Vector Sample)
combineMarkerSamples nss =
  (reifySamples nss) >>= (pure . combineFieldSamples)

combineFieldSamples
  :: NonEmpty (V.Vector Sample)
  -> V.Vector Sample
combineFieldSamples (s:|ss) =
  foldl' (V.zipWith combineSamples) s ss

-- This isn't a monoid instance or implemented in Warden.Data.Sample
-- because I think it's wrong to do this without a shuffle; we do it here
-- because we know we shuffle at the end and don't need to do it on every
-- append.
combineSamples
  :: Sample
  -> Sample
  -> Sample
combineSamples NoSample x =
  x
combineSamples x NoSample =
  x
combineSamples (Sample x) (Sample y) =
  Sample $ x VU.++ y

reifySamples
  :: NonEmpty (V.Vector NumericSummary)
  -> Either SampleError (NonEmpty (V.Vector Sample))
reifySamples nss =
  let
    sample NoNumericSummary = Nothing
    sample (NumericSummary _ _ _ _ _ samp) = Just samp
  in
  case identical (NE.toList $ numericIndicatorVec <$> nss) of
    False ->
      Left NumericFieldMismatch
    True ->
      pure $ (V.fromList . catMaybes . V.toList . V.map sample) <$> nss

identical
  :: Eq a
  => [a]
  -> Bool
identical [] =
  True
identical (x:xs) =
  let
    go _ [] = True
    go z [y] = y == z
    go z (y:ys) = and [y == z, go y ys]
  in
  go x xs 

numericIndicatorVec
  :: V.Vector NumericSummary
  -> V.Vector Bool
numericIndicatorVec ns =
  flip V.map ns $ \x -> case x of
    NoNumericSummary ->
      False
    NumericSummary _ _ _ _ _ _ ->
      True
