{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Criterion.Main
import           Criterion.Types

import           Data.Char (ord)
import           Data.List.NonEmpty (NonEmpty)

import           P

import           Pipes
import qualified Pipes.Prelude as PP

import           System.IO

import           Test.IO.Warden

import           Warden.Data
import           Warden.Rows
import           Warden.View

wardenBench :: [Benchmark] -> IO ()
wardenBench = defaultMainWith cfg
  where
    cfg = defaultConfig {
            reportFile = Just "dist/build/warden-bench.html"
          , csvFile = Just "dist/build/warden-bench.csv"
          }

prepareView :: IO (NonEmpty ViewFile)
prepareView = do
  vp <- generateView (RecordCount 1000) (GenSize 1) (LineSize 100)
  unsafeWarden $ traverseView vp

benchPipesDecode :: NonEmpty ViewFile -> IO ()
benchPipesDecode vfs = do
  bitbucket <- openFile "/dev/null" WriteMode
  unsafeWarden . runEffect $
        readSVView (Separator . fromIntegral $ ord '|') vfs
    >-> PP.map show
    >-> PP.toHandle bitbucket

main :: IO ()
main = wardenBench [
    env prepareView $ \ ~(vfs) ->
      bgroup "parsing" $ [ bench "decode/pipes-csv/1000" $ nfIO (benchPipesDecode vfs)
                         ]
  ]
