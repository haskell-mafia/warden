{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Criterion.Main
import           Criterion.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (ord)
import           Data.Conduit ((=$=), ($$))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as C
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import           Disorder.Corpus (muppets)

import           P

import           System.IO
import           System.IO.Temp (withTempDirectory)
import           System.Random.MWC (createSystemRandom)

import           Test.IO.Warden
import           Test.QuickCheck (vectorOf, arbitrary, elements, choose)
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Numeric
import           Warden.PII
import           Warden.Row
import           Warden.Row.Internal
import           Warden.View

wardenBench :: [Benchmark] -> IO ()
wardenBench = defaultMainWith cfg
  where
    cfg = defaultConfig {
            reportFile = Just "dist/build/warden-bench.html"
          , csvFile = Just "dist/build/warden-bench.csv"
          }

prepareView :: FilePath -> IO (NonEmpty ViewFile)
prepareView root = do
  vp <- generateView (Deterministic 271828) root (RecordCount 1000) (GenSize 1) (LineSize 100)
  unsafeWarden $ traverseView NoIncludeDotFiles vp

prepareRow :: IO [ByteString]
prepareRow =
  fmap getValidSVRow $ generate' (Deterministic 314159) (GenSize 30) $ validSVRow (Separator . fromIntegral $ ord '|') (FieldCount 200)

prepareSVParse :: IO [Row]
prepareSVParse = do
  ts <- generate' (Deterministic 12345) (GenSize 30) . vectorOf 1000 $ 
    validSVRow (Separator . fromIntegral $ ord '|') (FieldCount 200)
  pure $ fmap (SVFields . V.fromList . getValidSVRow) ts

prepareHashText :: IO [ByteString]
prepareHashText =
  generate' (Deterministic 54321) (GenSize 30) $ vectorOf 1000 arbitrary

prepareNumbers :: IO [Double]
prepareNumbers =
  generate' (Deterministic 2468) (GenSize 100) $ vectorOf 10000 arbitrary

prepareFolds :: IO ([Row], [ByteString], [ByteString], [ByteString], ByteString)
prepareFolds = (,,,,) <$> prepareSVParse <*> prepareHashText <*> preparePII <*> prepareNonPII <*> prepareByteString

prepareMeanDevAccs :: IO [MeanDevAcc]
prepareMeanDevAccs =
  generate' (Deterministic 8642) (GenSize 100) $ vectorOf 10000 arbitrary

prepareNumericStates :: IO [NumericState]
prepareNumericStates =
  generate' (Deterministic 9876) (GenSize 100) $ vectorOf 10000 arbitrary

preparePII :: IO [ByteString]
preparePII =
  fmap (fmap fst) $ generate' (Deterministic 9753) (GenSize 100) $ vectorOf 10000 genPII

prepareNonPII :: IO [ByteString]
prepareNonPII =
  fmap (fmap T.encodeUtf8) $ generate' (Deterministic 9753) (GenSize 100) $ vectorOf 10000 (elements muppets)

prepareByteString :: IO ByteString
prepareByteString =
  fmap BS.pack . generate' (Deterministic 1111) (GenSize 100) $ vectorOf 100 (choose (0, 255))

benchABDecode :: FileFormat -> NonEmpty ViewFile -> IO ()
benchABDecode ff vfs =
  let sep = Separator . fromIntegral $ ord '|'
      lb = LineBound 65536 in do
  bitbucket <- openFile "/dev/null" WriteMode
  unsafeWarden $
        readView' (decodeByteString ff sep lb) vfs
    =$= C.map (BSC.pack . show)
    $$  CB.sinkHandle bitbucket

benchFieldParse :: [ByteString] -> [FieldLooks]
benchFieldParse = fmap parseField

benchUpdateSVParseState :: [Row] -> IO SVParseState
benchUpdateSVParseState rs = do
  g <- createSystemRandom
  unsafeWarden $ foldM (updateSVParseState (TextFreeformThreshold 100) g (ReservoirSampling $ ReservoirSize 100) (PIIChecks $ MaxPIIObservations 100)) initialSVParseState rs

benchHashText :: [ByteString] -> [Int]
benchHashText = fmap hashText

benchUpdateTextCounts :: [Row] -> TextCounts
benchUpdateTextCounts rs = foldl' (flip (updateTextCounts (TextFreeformThreshold 100))) NoTextCounts rs

benchUpdateNumericState :: [Double] -> NumericState
benchUpdateNumericState ns = foldl' updateNumericState initialNumericState ns

benchCombineMeanDevAcc :: [MeanDevAcc] -> MeanDevAcc
benchCombineMeanDevAcc mdas = foldl' combineMeanDevAcc MeanDevInitial mdas

benchCombineNumericState :: [NumericState] -> NumericState
benchCombineNumericState nss = foldl' combineNumericState initialNumericState nss

benchUpdateFieldPIIObservations :: [ByteString] -> PIIObservations
benchUpdateFieldPIIObservations bss = foldl' (updatePIIObservations (MaxPIIObservations 100000) (FieldIndex 1)) NoPIIObservations bss

benchCheckPII :: [ByteString] -> [Maybe PIIType]
benchCheckPII = fmap checkPII

main :: IO ()
main = do
  withTempDirectory "." "warden-bench-" $ \root ->
    wardenBench [
          env (prepareView root) $ \ ~(vfs) ->
            bgroup "decoding" $ [
                bench "decode/rfc4180/1000" $ nfIO (benchABDecode RFC4180 vfs)
              , bench "decode/delimited-text/1000" $ nfIO (benchABDecode DelimitedText vfs)
            ]
        , env prepareRow $ \ ~(rs) ->
            bgroup "field-parsing" $ [
                bench "parseField/200" $ nf benchFieldParse rs
            ]
        , env prepareFolds $ \ ~(rs, ts, piis, nonPiis, bs) ->
            bgroup "folds" $ [
                bench "updateSVParseState/1000" $ nfIO (benchUpdateSVParseState rs)
              , bench "hashText/1000" $ nf benchHashText ts
              , bench "updateTextCounts/1000" $ nf benchUpdateTextCounts rs
              , bench "updatePIIObservations/pii/10000" $ nf benchUpdateFieldPIIObservations piis
              , bench "updatePIIObservations/nonpii/10000" $ nf benchUpdateFieldPIIObservations nonPiis
              , bench "checkPII/pii/10000" $ nf benchCheckPII piis
              , bench "checkPII/nonpii/10000" $ nf benchCheckPII nonPiis
              , bench "asciiToLower/100" $ nf asciiToLower bs
            ]
        , env ((,,) <$> prepareNumbers <*> prepareMeanDevAccs <*> prepareNumericStates) $ \ ~(ns, mdas, nss) ->
           bgroup "numerics" $ [
                bench "updateNumericState/10000" $ nf benchUpdateNumericState ns
             ,  bench "combineMeanDevAcc/10000" $ nf benchCombineMeanDevAcc mdas
             ,  bench "combineNumericState/10000" $ nf benchCombineNumericState nss
             ]
        ]
