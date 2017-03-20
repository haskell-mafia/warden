{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_warden
import           DependencyInfo_ambiata_warden

import           Options.Applicative (Parser)
import           Options.Applicative (subparser)

import           P

import           System.IO (IO, BufferMode(..))
import           System.IO (stdout, stderr, hSetBuffering)
import           System.IO (hPutStrLn)
import           System.Exit (exitFailure)

import           X.Options.Applicative (cli, command')

data Command =
    Instantaneous
  | Temporal
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "warden-anomalies" buildInfoVersion dependencyInfo commandP $ \cmd ->
    case cmd of
      Instantaneous -> do
        hPutStrLn stderr $ "implement me!"
        exitFailure
      Temporal -> do
        hPutStrLn stderr $ "implement me!"
        exitFailure

commandP :: Parser Command
commandP = subparser $
     command' "instantaneous" "Run anomaly detection on a single dataset with no temporal information." instantaneousP
  <> command' "temporal" "Run time-series anomaly detection on a dataset." temporalP

instantaneousP :: Parser Command
instantaneousP =
  pure Instantaneous

temporalP :: Parser Command
temporalP =
  pure Temporal
