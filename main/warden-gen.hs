{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

import           P

import           System.Exit
import           System.IO (IO, print, putStrLn)

import           X.Options.Applicative

newtype RecordCount =
  RecordCount Int
  deriving (Eq, Show)

data Command = Generate RecordCount
  deriving (Eq, Show)

main :: IO ()
main = do
  dispatch (safeCommand wardenGenP) >>= \case
    VersionCommand -> do
      exitSuccess
    RunCommand DryRun c -> do
      print c
      exitSuccess
    RunCommand RealRun _ -> do
      putStrLn "implement me!"
      exitFailure

wardenGenP :: Parser Command
wardenGenP = subparser $
     command' "gen" "Generate a view for testing/benchmarking." generateP

generateP :: Parser Command
generateP = Generate <$> recordCountP

recordCountP :: Parser RecordCount
recordCountP = RecordCount <$> (option auto $
     long "record-count"
  <> short 'c'
  <> metavar "COUNT"
  <> value 1000000
  <> help "Number of records to generate (default 10^6).")
