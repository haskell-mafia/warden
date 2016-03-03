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

import           P

import           System.IO
import           System.IO.Temp (withTempDirectory)

import           Test.IO.Warden

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

benchConduitDecode :: NonEmpty ViewFile -> IO ()
benchConduitDecode vfs = do
  bitbucket <- openFile "/dev/null" WriteMode
  unsafeWarden $
        readView (Separator . fromIntegral $ ord '|') (LineBound 65536) vfs
    =$= C.map (BS.pack . show)
    $$  CB.sinkHandle bitbucket

main :: IO ()
main = do
  withTempDirectory "." "warden-bench-" $ \root ->
    wardenBench [
        env (prepareView root) $ \ ~(vfs) ->
          bgroup "parsing" $ [
              bench "decode/conduit+cassava/1000" $ nfIO (benchConduitDecode vfs)
            ]
        ]
