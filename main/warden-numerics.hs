{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_warden
import           DependencyInfo_ambiata_warden

import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Char (ord)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T
import qualified Data.Text as T

import           Options.Applicative

import           P

import           System.Exit (exitSuccess, exitFailure)
import           System.IO (IO, FilePath, print, putStrLn)

import           Warden.Commands
import           Warden.Data
import           Warden.Error
import           Warden.Param
import           Warden.Schema

import           X.Control.Monad.Trans.Either (mapEitherT)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative

data Command =
    Simulate
  deriving (Eq, Show)

main :: IO ()
main = do
  dispatch (safeCommand wardenP) >>= \case
    VersionCommand -> do
      putStrLn ("warden: " <> buildInfoVersion)
    DependencyCommand -> do
      mapM_ putStrLn dependencyInfo
    RunCommand DryRun c -> do
      print c
      exitSuccess
    RunCommand RealRun cmd -> do
      wps <- buildWardenParams . WardenVersion $ T.pack buildInfoVersion
      run wps cmd

run :: Command -> IO ()
run Simulate = do
  pure ()

wardenP :: Parser Command
wardenP = subparser $
     command' "simulate" "Run simulations of numerical computations for accuracy evaluation." (pure Simulate)
