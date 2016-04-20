{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Row where

import           Control.Lens ((^.))
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString.Lazy as BL
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import           Data.Csv
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import           Disorder.Core.IO

import           P

import           System.IO
import           System.Random.MWC (withSystemRandom)

import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()
import           Test.IO.Warden
import           Test.Warden
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Chunk
import           Warden.Error
import           Warden.Row

import           X.Control.Monad.Trans.Either

prop_updateSVParseState :: TextFreeformThreshold -> SamplingType -> [ValidRow] -> Property
prop_updateSVParseState fft st rs = testIO . withSystemRandom $ \g -> unsafeWarden $ do
  let rs' = unValidRow <$> rs
  s <- foldM (updateSVParseState fft g st) initialSVParseState rs'
  pure $ (s ^. badRows, s ^. totalRows) === (RowCount 0, RowCount . fromIntegral $ length rs)

prop_resolveSVParseState :: TextFreeformThreshold -> SamplingType -> [Blind SVParseState] -> Property
prop_resolveSVParseState fft st ss = testIO . withSystemRandom $ \g -> do
  s' <- resolveSVParseState fft g st $ getBlind <$> ss
  let bad' = s' ^. badRows
  let total' = s' ^. totalRows
  let fns' = S.size $ s' ^. numFields
  pure . conjoin . fmap (\s'' ->    bad' >= (s'' ^. badRows)
                                 && total' >= (s'' ^. totalRows)
                                 && fns' >= (S.size $ s'' ^. numFields)) $ getBlind <$> ss

prop_valid_svrows :: Separator -> FieldCount -> Property
prop_valid_svrows s i = forAll (choose (1, 100)) $ \n -> forAll (vectorOf n $ validSVRowQuotes s i) $ \svrs ->
  testIO $ withTestViewFile $ \vf -> do
    let fp = viewFilePath vf
    BL.writeFile fp $ encodeWith (wardenEncodeOpts s) svrs
    res <- runEitherT . mapEitherT runResourceT $ readViewFile s (LineBound 65536) vf $$ CL.fold (flip (:)) []
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right rs -> do
        let expected = reverse $ (stripFieldQuotes . SVFields . V.fromList . getValidSVRow) <$> svrs
        pure $ expected === (stripFieldQuotes <$> rs)

prop_valid_svrows_chunked :: ChunkCount -> Separator -> FieldCount -> Property
prop_valid_svrows_chunked cc s i = forAll (choose (1, 100)) $ \n -> forAll (vectorOf n $ validSVRowQuotes s i) $ \svrs ->
  testIO $ withTestViewFile $ \vf -> do
    let fp = viewFilePath vf
    BL.writeFile fp $ encodeWith (wardenEncodeOpts s) svrs
    cs <- chunk cc fp
    res <- runEitherT . mapEitherT runResourceT $ fmap (join . NE.toList) $ 
      mapM (\c -> readViewChunk s (LineBound 65536) vf c $$ CL.fold (flip (:)) []) cs
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right rs -> do
        let expected = reverse $ (stripFieldQuotes . SVFields . V.fromList . getValidSVRow) <$> svrs
        pure $ expected === (stripFieldQuotes <$> rs)

prop_invalid_svrows :: Separator -> Property
prop_invalid_svrows s = forAll (choose (1, 100)) $ \n -> forAll (vectorOf n (invalidSVRow s)) $ \svrs ->
  testIO $ withTestViewFile $ \vf -> do
    let fp = viewFilePath vf
    BL.writeFile fp $ (BL.intercalate "\r\n") svrs
    res <- runEitherT . mapEitherT runResourceT $ readViewFile s (LineBound 65536) vf $$ CL.fold (flip (:)) []
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right rs ->
        pure $ [] === filter (not . rowFailed) rs
 where
  rowFailed (RowFailure _) = True
  rowFailed _              = False

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 })
