{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_warden
import           DependencyInfo_ambiata_warden

import           Options.Applicative (Parser)
import           Options.Applicative (subparser)

import           P

import           System.IO (IO, BufferMode(..))
import           System.IO (stdout, stderr, hSetBuffering)

import           X.Options.Applicative (cli, command')

data Command =
    Simulate
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "warden-numerics" buildInfoVersion dependencyInfo commandP $ \cmd ->
    case cmd of
      Simulate ->
        pure ()

commandP :: Parser Command
commandP = subparser $
     command' "simulate" "Run simulations of numerical computations for accuracy evaluation." (pure Simulate)
