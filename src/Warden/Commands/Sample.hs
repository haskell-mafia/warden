{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Commands.Sample (
    extractNumericFields
  , summariseNumericFields
  , readNumericSummary
  , identical
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)

import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           P

import           System.IO (IO, FilePath, Handle, IOMode(..))
import           System.IO (withFile)

import           Warden.Data.Marker
import           Warden.Data.Numeric
import           Warden.Data.Sampling
import           Warden.Error
import           Warden.Marker

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (left, hoistEither)
import qualified X.Data.Vector.Generic as XV

summariseNumericFields
  :: FilePath
  -> [FilePath]
  -> EitherT WardenError (ResourceT IO) ()
summariseNumericFields _outd _fs =
  left WardenNotImplementedError

extractNumericFields
  :: FilePath
  -> [FilePath]
  -> EitherT WardenError (ResourceT IO) ()
extractNumericFields outp fs =
  case nonEmpty fs of
    Nothing ->
      pure ()
    Just fs' -> do
      nss <- mapM readNumericSummary fs'
      ss <- hoistEither . first WardenSampleError $ combineMarkerSamples nss
      writeSamples outp ss

writeSamples
  :: FilePath
  -> V.Vector Sample
  -> EitherT WardenError (ResourceT IO) ()
writeSamples fp ss =
  let
    ssT = transposeSamples ss
  in
  liftIO $ withFile fp WriteMode $ \h ->
    V.mapM_ (writeRow h) ssT

writeRow
  :: Handle
  -> VU.Vector Double
  -> IO ()
writeRow h xs =
  let
    row = T.encodeUtf8 . T.intercalate "," . fmap renderFractional $ VU.toList xs
  in do
  BS.hPut h row
  BS.hPut h "\n"

-- | Transpose sample vector from column-major (vector of samples, as
-- they're stored in the view metadata) to row-major (vector of
-- records) for output.
transposeSamples :: V.Vector Sample -> V.Vector (VU.Vector Double)
transposeSamples ss =
  let
    reify (Sample xs) = Just xs
    reify NoSample = Nothing
  in
  XV.transpose $ XV.mapMaybe reify ss

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
      pure $ (XV.mapMaybe sample) <$> nss

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
