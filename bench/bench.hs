{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Criterion.Main
import           Criterion.Types

import qualified Data.ByteString.Char8 as BS
import           Data.Char (ord)
import           Data.Conduit ((=$=), ($$))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as C
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Data.Vector as V

import           P

import           System.IO
import           System.IO.Temp (withTempDirectory)

import           Test.IO.Warden
import           Test.QuickCheck (vectorOf, arbitrary)
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Row
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
  unsafeWarden $ traverseView vp

prepareRow :: IO [Text]
prepareRow =
  fmap getValidSVRow $ generate' (Deterministic 314159) (GenSize 30) $ validSVRow (Separator . fromIntegral $ ord '|') (FieldCount 200)

prepareSVParse :: IO [Row]
prepareSVParse = do
  ts <- generate' (Deterministic 12345) (GenSize 30) . vectorOf 1000 $ 
    validSVRow (Separator . fromIntegral $ ord '|') (FieldCount 200)
  pure $ fmap (SVFields . V.fromList . getValidSVRow) ts

prepareHashText :: IO [Text]
prepareHashText =
  generate' (Deterministic 54321) (GenSize 30) $ vectorOf 1000 arbitrary

prepareFolds :: IO ([Row], [Text])
prepareFolds = (,) <$> prepareSVParse <*> prepareHashText

benchConduitDecode :: NonEmpty ViewFile -> IO ()
benchConduitDecode vfs = do
  bitbucket <- openFile "/dev/null" WriteMode
  unsafeWarden $
        readView (Separator . fromIntegral $ ord '|') (LineBound 65536) vfs
    =$= C.map (BS.pack . show)
    $$  CB.sinkHandle bitbucket

benchFieldParse :: [Text] -> [FieldLooks]
benchFieldParse = fmap parseField

benchUpdateSVParseState :: [Row] -> SVParseState
benchUpdateSVParseState rs = foldl' updateSVParseState initialSVParseState rs

benchHashText :: [Text] -> [Int]
benchHashText = fmap hashText

main :: IO ()
main = do
  withTempDirectory "." "warden-bench-" $ \root ->
    wardenBench [
          env (prepareView root) $ \ ~(vfs) ->
            bgroup "decoding" $ [
                bench "decode/conduit+cassava/1000" $ nfIO (benchConduitDecode vfs)
            ]
        , env prepareRow $ \ ~(rs) ->
            bgroup "field-parsing" $ [
                bench "parseField/200" $ nf benchFieldParse rs
            ]
        , env prepareFolds $ \ ~(rs,ts) ->
            bgroup "folds" $ [
                bench "updateSVParseState/1000" $ nf benchUpdateSVParseState rs
              , bench "hashText/1000" $ nf benchHashText ts
            ]
        ]
