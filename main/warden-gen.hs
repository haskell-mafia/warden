{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Text as T

import           Disorder.Corpus (muppets)

import           P

import           System.Exit
import           System.FilePath ((</>))
import           System.IO (IO, print)

import           Test.IO.Warden
import           Test.QuickCheck (generate, arbitrary, suchThat, elements)
import           Test.Warden.Arbitrary

import           Warden.Data

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
    RunCommand RealRun (Generate c) -> do
      generateView c

generateView :: RecordCount -> IO ()
generateView (RecordCount _) = do
  dt <- generate . fmap unValidDirTree $ arbitrary `suchThat` ((> 0) . length . directoryFiles . unValidDirTree)
  tok <- generate $ elements muppets
  writeView ("./warden-gen" </> T.unpack tok) dt

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
